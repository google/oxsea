// Copyright 2024 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::{collections::HashMap, panic};

use oxc_ast::{
    ast::{AssignmentTarget, BinaryOperator, BindingPatternKind, Program, TSModuleDeclaration},
    visit::walk::walk_program,
    AstKind, Visit,
};
use oxc_index::IndexVec;
use oxc_semantic::{ReferenceFlags, Semantic, SemanticBuilder, SymbolId};
use oxsea_ir::{CompileTimeValue, IRGraph, IRNodeId, IR_END_ID, IR_START_ID};

struct FromAST<'i> {
    ir: IRGraph,
    _program: &'i Program<'i>,
    _semantic: &'i Semantic<'i>,

    control_tail: IRNodeId,
    value_stack: Vec<IRNodeId>,

    symbol_table: IndexVec<SymbolId, IRNodeId>,
    named_exports: HashMap<SymbolId, String>,
}

impl<'a> Visit<'a> for FromAST<'a> {
    fn leave_node(&mut self, kind: AstKind<'a>) {
        match kind {
            AstKind::NumericLiteral(l) => {
                self.value_stack
                    .push(self.ir.add_constant(CompileTimeValue::Number(l.value)));
            }
            AstKind::BooleanLiteral(l) => {
                self.value_stack
                    .push(self.ir.add_constant(CompileTimeValue::Boolean(l.value)));
            }
            AstKind::BinaryExpression(b) => match b.operator {
                oxc_ast::ast::BinaryOperator::Addition => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.value_stack.push(self.ir.add_add(left, right));
                }
                BinaryOperator::StrictEquality | BinaryOperator::Equality => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.value_stack
                        .push(self.ir.add_compare(left, right, b.operator));
                }
                _ => {
                    panic!(
                        "BinaryExpression with unsupported operator: {:?}",
                        b.operator
                    );
                }
            },
            AstKind::ReturnStatement(_) => {
                let value = self.value_stack.pop().unwrap();
                self.control_tail = self.ir.add_return(self.control_tail, value);
            }
            AstKind::VariableDeclaration(v) => {
                // Ignore declare-only cases, they only matter for type checking.
                if v.declare {
                    return;
                }

                // This is a severe oversimplification of what's _actually_ required.
                if v.declarations.len() != 1 {
                    panic!("VariableDeclaration with multiple declarations");
                }
                for decl in &v.declarations {
                    if decl.init.is_none() {
                        panic!("VariableDeclaration without init, not handled yet");
                    }

                    match &decl.id.kind {
                        BindingPatternKind::BindingIdentifier(b) => {
                            let symbol = b.symbol_id();
                            let value = self.value_stack.pop().unwrap();
                            self.symbol_table[symbol] = value;
                        }
                        _ => {
                            panic!("VariableDeclaration with non-binding identifier");
                        }
                    }
                }
            }
            AstKind::IdentifierReference(r) => {
                let reference = self._semantic.symbols().get_reference(r.reference_id());
                if reference.flags().contains(ReferenceFlags::Read) {
                    if let Some(symbol_id) = reference.symbol_id() {
                        let mut value = self.symbol_table[symbol_id];
                        if value == 0 {
                            let symbol_name = self._semantic.symbols().get_name(symbol_id);
                            value = self.ir.add_load_global(symbol_name.to_string());
                        }
                        self.value_stack.push(value);
                    } else {
                        // This would happen when reading an undeclared global variable.
                        let value = self.ir.add_load_global(r.name.to_string());
                        self.value_stack.push(value);
                    }
                }
            }
            AstKind::AssignmentExpression(a) => {
                if !a.operator.is_assign() {
                    panic!("AssignmentExpression with non-assign operator");
                }
                let value = self.value_stack.pop().unwrap();
                match &a.left {
                    AssignmentTarget::AssignmentTargetIdentifier(r) => {
                        let reference = self._semantic.symbols().get_reference(r.reference_id());
                        if let Some(symbol) = reference.symbol_id() {
                            self.symbol_table[symbol] = value;
                        } else {
                            panic!("AssignmentExpression with no symbol");
                        }
                    }
                    _ => {
                        panic!("AssignmentExpression with non-binding identifier");
                    }
                }
            }
            AstKind::ExportNamedDeclaration(e) => {
                // Ignore non-value exports, they are for type checking only.
                if !e.export_kind.is_value() {
                    return;
                }

                // Find all symbol ids that are being declared.
                if let Some(decl) = &e.declaration {
                    match decl {
                        oxc_ast::ast::Declaration::VariableDeclaration(v) => {
                            for decl in &v.declarations {
                                match &decl.id.kind {
                                    BindingPatternKind::BindingIdentifier(b) => {
                                        let symbol = b.symbol_id();
                                        self.named_exports.insert(symbol, b.name.to_string());
                                    }
                                    _ => {
                                        panic!("VariableDeclaration with non-binding identifier");
                                    }
                                }
                            }
                        }
                        _ => {
                            panic!("ExportNamedDeclaration with non-VariableDeclaration");
                        }
                    }
                }
            }
            AstKind::ExportDefaultDeclaration(_) => {
                let value = self.value_stack.pop().unwrap();
                self.ir
                    .add_bind_export(self.control_tail, "default".to_string(), value);
            }
            _ => { /* Ignore other nodes. */ }
        }
    }

    fn visit_ts_module_declaration(&mut self, _it: &TSModuleDeclaration<'a>) {
        /* Ignore contents of `declare ...` blocks. */
    }

    fn visit_if_statement(&mut self, it: &oxc_ast::ast::IfStatement<'a>) {
        let control_input = self.control_tail;
        // First, collect the node for our test expression.
        self.visit_expression(&it.test);
        let condition = self.value_stack.pop().unwrap();
        let branch_id = self.ir.add_if_else(control_input, condition);
        let consequent_id = self.ir.add_proj(branch_id, 0);
        let alternate_id = self.ir.add_proj(branch_id, 1);

        let original_symbol_table = self.symbol_table.clone();

        self.control_tail = consequent_id;
        self.visit_statement(&it.consequent);
        let consequent_tail = self.control_tail;
        let consequent_symbol_table = self.symbol_table.clone();

        let mut alternate_tail = alternate_id;
        self.symbol_table = original_symbol_table;
        if let Some(alternate) = &it.alternate {
            self.control_tail = alternate_id;
            self.visit_statement(alternate);
            alternate_tail = self.control_tail;
        }

        let merge_id = self
            .ir
            .add_merge(branch_id, &[consequent_tail, alternate_tail]);

        for (symbol, value) in self.symbol_table.iter_mut_enumerated() {
            let consequent_value = consequent_symbol_table[symbol];
            if consequent_value != *value {
                // If the symbol has diverged, create a phi node for it.
                let phi = self.ir.add_phi(merge_id, &[consequent_value, *value]);
                *value = phi;
            }
        }

        self.control_tail = merge_id;
    }
}

impl<'i> FromAST<'i> {
    pub fn new(program: &'i Program, semantic: &'i Semantic<'i>) -> Self {
        let mut symbol_table = IndexVec::new();
        symbol_table.resize(semantic.symbols().len(), 0);
        Self {
            ir: IRGraph::new(),
            _program: program,
            _semantic: semantic,

            control_tail: IR_START_ID,
            value_stack: vec![],

            symbol_table: symbol_table,
            named_exports: HashMap::new(),
        }
    }

    fn link_exports(&mut self) {
        let root_scope_id = self._semantic.scopes().root_scope_id();
        for (scope_id, symbol_id, _) in self._semantic.scopes().iter_bindings() {
            if scope_id != root_scope_id {
                continue;
            }
            if let Some(export_name) = self.named_exports.get(&symbol_id) {
                let value = self.symbol_table[symbol_id];
                self.ir
                    .add_bind_export(self.control_tail, export_name.clone(), value);
            }
        }
    }

    pub fn try_from_ast(&mut self) -> Result<(), &'static str> {
        walk_program(self, self._program);
        if self.value_stack.len() != 0 {
            return Err("Value stack is not empty");
        }
        self.link_exports();
        self.ir.add_edge(self.control_tail, IR_END_ID);
        Ok(())
    }
}

pub fn ir_from_ast<'a>(program: &Program) -> Result<IRGraph, &'static str> {
    let semantic_result = SemanticBuilder::new()
        .with_check_syntax_error(true)
        .with_cfg(true)
        .build(&program);
    if !semantic_result.errors.is_empty() {
        return Err("Analysis failed");
    }
    let semantic = semantic_result.semantic;

    let mut from_ast = FromAST::new(&program, &semantic);

    from_ast.try_from_ast()?;

    Ok(from_ast.ir)
}

#[cfg(test)]
mod tests {
    use oxc_allocator::Allocator;
    use oxc_ast::ast::SourceType;
    use oxc_parser::Parser;
    use oxsea_ir::{CompileTimeValue, IRNode, IR_END_ID};

    fn ir_from_source(source: &str) -> oxsea_ir::IRGraph {
        let allocator = Allocator::default();
        let ret = Parser::new(&allocator, source, SourceType::tsx()).parse();
        let ir = super::ir_from_ast(&ret.program).unwrap();
        check_graph(&ir);
        ir
    }

    #[test]
    fn default_export() {
        let actual = ir_from_source("export default 13;");
        assert_eq!(
            actual.get_node(2),
            &IRNode::new(
                oxsea_ir::IRInstruction::Constant(CompileTimeValue::Number(13.0)),
                vec![],
                vec![3],
            )
        );
        assert_eq!(
            actual.get_node(3),
            &IRNode::new(
                oxsea_ir::IRInstruction::BindExport("default".to_string()),
                vec![1, 2],
                vec![IR_END_ID],
            )
        );
    }

    fn check_graph(ir: &oxsea_ir::IRGraph) {
        // Check that all inputs and outputs are linked properly.
        for node_id in 0..ir.len() {
            let node = ir.get_node(node_id);
            for &input in node.inputs() {
                assert!(
                    ir.get_node(input).outputs().contains(&node_id),
                    "Node: #{} {:#?}\nIs missing from outputs of #{} {:#?}\n",
                    node_id,
                    node,
                    input,
                    ir.get_node(input)
                );
            }
            for &output in node.outputs() {
                assert!(
                    ir.get_node(output).inputs().contains(&node_id),
                    "Node: #{} {:#?}\nIs missing from inputs of #{} {:#?}\n",
                    node_id,
                    node,
                    output,
                    ir.get_node(output)
                );
            }
        }
    }

    #[test]
    fn pruned_branch() {
        ir_from_source("let x = 1; if (true) { x = 2; } export default x;");
    }

    #[test]
    fn it_needs_work() {
        let actual = ir_from_source("return 43 + 2;");
        assert_eq!(
            actual.get_node(2),
            &IRNode::new(
                oxsea_ir::IRInstruction::Constant(CompileTimeValue::Number(43.0)),
                vec![],
                vec![],
            )
        );
        assert_eq!(
            actual.get_node(3),
            &IRNode::new(
                oxsea_ir::IRInstruction::Constant(CompileTimeValue::Number(2.0)),
                vec![],
                vec![],
            )
        );
        assert_eq!(
            actual.get_node(4),
            &IRNode::new(
                oxsea_ir::IRInstruction::Constant(CompileTimeValue::Number(45.0)),
                vec![],
                vec![5],
            )
        );
        assert_eq!(
            actual.get_node(5),
            &IRNode::new(oxsea_ir::IRInstruction::Return, vec![1, 4], vec![0],)
        );
    }
}
