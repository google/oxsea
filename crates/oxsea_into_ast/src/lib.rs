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

use oxc_allocator::{Allocator, Vec};
use oxc_ast::ast::BindingPatternKind::BindingIdentifier;
use oxc_ast::ast::Declaration::VariableDeclaration;
use oxc_ast::ast::{
    AssignmentOperator, AssignmentTarget, BinaryOperator, BindingRestElement, Expression,
    FormalParameterKind, FunctionType, ImportOrExportKind, NumberBase, Program, SourceType, Span,
    Statement, TSThisParameter, TSTypeAnnotation, TSTypeParameterDeclaration,
    VariableDeclarationKind, WithClause,
};
use oxc_ast::AstBuilder;
use oxsea_ir::{CompileTimeValue, IRGraph, IRInstruction, IRNode, IRNodeId};

struct IntoAST<'i, 'o> {
    graph: &'i IRGraph,
    ast: AstBuilder<'o>,
}

impl<'i, 'o> IntoAST<'i, 'o> {
    pub fn new(allocator: &'o Allocator, graph: &'i IRGraph) -> Self {
        Self {
            graph: &graph,
            ast: AstBuilder::new(allocator),
        }
    }

    fn visit_expression(&self, node_id: IRNodeId) -> Result<Expression<'o>, &'static str> {
        let node = self.graph.get_node(node_id);
        self.visit_expression_with_node(node_id, node)
    }

    fn visit_expression_with_node(
        &self,
        node_id: IRNodeId,
        node: &IRNode,
    ) -> Result<Expression<'o>, &'static str> {
        match &node.instruction() {
            IRInstruction::Constant(v) => match v {
                CompileTimeValue::Number(n) => Ok(Expression::NumericLiteral(
                    self.ast
                        .alloc_numeric_literal(Span::empty(0), *n, None, NumberBase::Decimal),
                )),
                CompileTimeValue::Boolean(b) => Ok(Expression::BooleanLiteral(
                    self.ast.alloc_boolean_literal(Span::empty(0), *b),
                )),
                _ => Err("Not implemented - CompileTimeValue"),
            },
            IRInstruction::LoadGlobal(name) => Ok(Expression::Identifier(
                self.ast
                    .alloc_identifier_reference(Span::empty(0), name.clone()),
            )),
            IRInstruction::Add => {
                let left_node = node.inputs()[0];
                let right_node = node.inputs()[1];
                let left = self.visit_expression(left_node)?;
                let right = self.visit_expression(right_node)?;
                Ok(Expression::BinaryExpression(
                    self.ast.alloc_binary_expression(
                        Span::empty(0),
                        left,
                        BinaryOperator::Addition,
                        right,
                    ),
                ))
            }
            IRInstruction::Phi => Ok(Expression::Identifier(
                self.ast
                    .alloc_identifier_reference(Span::empty(0), format!("Φ{}", node_id)),
            )),
            IRInstruction::Compare(operator) => {
                let left_node = node.inputs()[0];
                let right_node = node.inputs()[1];
                let left = self.visit_expression(left_node)?;
                let right = self.visit_expression(right_node)?;
                Ok(Expression::BinaryExpression(
                    self.ast
                        .alloc_binary_expression(Span::empty(0), left, *operator, right),
                ))
            }
            _ => {
                println!("{:?}", node.instruction());
                Err("Not implemented - visit_expression")
            }
        }
    }

    fn visit_statement(&self, node_id: IRNodeId) -> Result<Option<Statement<'o>>, &'static str> {
        let node = self.graph.get_node(node_id);
        self.visit_statement_with_node(node_id, node)
    }

    fn visit_statement_with_node(
        &self,
        node_id: IRNodeId,
        node: &IRNode,
    ) -> Result<Option<Statement<'o>>, &'static str> {
        match &node.instruction() {
            IRInstruction::Return => {
                let value = node.inputs()[1];
                let value_expr: Expression<'o> = self.visit_expression(value)?;
                Ok(Some(Statement::ReturnStatement(
                    self.ast
                        .alloc_return_statement(Span::empty(0), Some(value_expr)),
                )))
            }
            IRInstruction::BindExport(name) => {
                let value = node.inputs()[0];
                let value_expr: Expression<'o> = self.visit_expression(value)?;
                if name == "default" {
                    return Ok(Some(Statement::ExportDefaultDeclaration(
                        self.ast.alloc_export_default_declaration(
                            Span::empty(0),
                            value_expr.into(),
                            self.ast.module_export_name_identifier_name(
                                Span::empty(0),
                                "default".to_string(),
                            ),
                        ),
                    )));
                } else {
                    // panic!("Not implemented - BindExport: {:?}", name);
                    return Ok(Some(Statement::ExportNamedDeclaration(
                        self.ast.alloc_export_named_declaration(
                            Span::empty(0),
                            Some(VariableDeclaration(self.ast.alloc_variable_declaration(
                                Span::empty(0),
                                VariableDeclarationKind::Let,
                                self.ast.vec1(self.ast.variable_declarator(
                                    Span::empty(0),
                                    VariableDeclarationKind::Let,
                                    self.ast.binding_pattern(
                                        BindingIdentifier(
                                            self.ast.alloc_binding_identifier(Span::empty(0), name),
                                        ),
                                        None::<TSTypeAnnotation<'_>>,
                                        false,
                                    ),
                                    value_expr.into(),
                                    false,
                                )),
                                false,
                            ))),
                            self.ast.vec(),
                            None,
                            ImportOrExportKind::Value,
                            None::<WithClause<'_>>,
                        ),
                    )));
                }
            }
            IRInstruction::Start => Ok(None),
            IRInstruction::IfElse => {
                let test_id = node.inputs()[1];
                let test = self.visit_expression(test_id)?;
                let consequent_id = node.outputs()[0];
                let alternate_id = node.outputs()[1];
                let consequent = self.visit_statement(consequent_id)?;
                let alternate = self.visit_statement(alternate_id)?;
                if let Some(consequent) = consequent {
                    return Ok(Some(Statement::IfStatement(self.ast.alloc_if_statement(
                        Span::empty(0),
                        test,
                        consequent,
                        alternate,
                    ))));
                }
                panic!("Not implemented - IfElse without consequent");
            }
            IRInstruction::Block => {
                let mut body = self.ast.vec();
                for (index, output) in node.outputs().iter().enumerate() {
                    if index == 0 {
                        continue;
                    }
                    let output_node = self.graph.get_node(*output);
                    if let IRInstruction::Phi = output_node.instruction() {
                        let phi_input_index = output_node
                            .inputs()
                            .iter()
                            .position(|x| *x == node_id)
                            .unwrap();
                        println!("Set phi Φ{} @ {}", output, phi_input_index);
                        let value = self.visit_expression(output_node.inputs()[phi_input_index + 1])?;
                        body.push(Statement::ExpressionStatement(
                            self.ast.alloc_expression_statement(
                                Span::empty(0),
                                Expression::AssignmentExpression(
                                    self.ast.alloc_assignment_expression(
                                        Span::empty(0),
                                        AssignmentOperator::Assign,
                                        AssignmentTarget::AssignmentTargetIdentifier(
                                            self.ast.alloc_identifier_reference(
                                                Span::empty(0),
                                                format!("Φ{}", *output)
                                            ),
                                        ),
                                        value,
                                    ),
                                ),
                            ),
                        ));
                        continue;
                    }
                    let stmt = self.visit_statement(*output)?;
                    if let Some(stmt) = stmt {
                        body.push(stmt);
                    }
                }
                return Ok(Some(Statement::BlockStatement(
                    self.ast.alloc_block_statement(Span::empty(0), body),
                )));
            }
            IRInstruction::Merge => {
                // First input is the branch point, go there.
                return self.visit_statement(node.inputs()[0]);
            }
            _ => {
                println!("{:?}", node.instruction());
                Err("Not implemented - visit_statement")
            }
        }
    }

    fn visit_end(&self, node: &IRNode) -> Result<Vec<'o, Statement<'o>>, &'static str> {
        let mut stmts = self.ast.vec();
        for input in node.inputs() {
            let stmt = self.visit_statement(*input)?;
            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
        }
        stmts.reverse();
        Ok(stmts)
    }

    fn wrap_in_function_declaration(
        &self,
        stmts: Vec<'o, Statement<'o>>,
    ) -> Vec<'o, Statement<'o>> {
        self.ast.vec1(Statement::FunctionDeclaration(
            self.ast.alloc_function(
                FunctionType::FunctionDeclaration,
                Span::empty(0),
                Some(self.ast.binding_identifier(Span::empty(0), "f")),
                false,
                false,
                false,
                None::<oxc_allocator::Box<'o, TSTypeParameterDeclaration<'o>>>,
                None::<TSThisParameter<'o>>,
                self.ast.alloc_formal_parameters(
                    Span::empty(0),
                    FormalParameterKind::FormalParameter,
                    self.ast.vec(),
                    None::<BindingRestElement<'o>>,
                ),
                None::<TSTypeAnnotation<'o>>,
                Some(
                    self.ast
                        .alloc_function_body(Span::empty(0), self.ast.vec(), stmts),
                ),
            ),
        ))
    }

    fn visit_end_and_wrap(
        &self,
        node: &IRNode,
        as_func: bool,
    ) -> Result<Vec<'o, Statement<'o>>, &'static str> {
        let stmts = self.visit_end(node)?;
        if as_func {
            Ok(self.wrap_in_function_declaration(stmts))
        } else {
            Ok(stmts)
        }
    }

    pub fn try_into_ast(&self, as_func: bool) -> Result<Program<'o>, &'static str> {
        let stmts = self.visit_end_and_wrap(self.graph.end_node(), as_func)?;
        // Generate AST from IR graph
        return Ok(self.ast.program(
            Span::empty(0),
            SourceType::tsx(),
            "",
            self.ast.vec(),
            None,
            self.ast.vec(),
            stmts,
        ));
    }
}

pub fn ir_into_ast<'a>(
    allocator: &'a Allocator,
    graph: &IRGraph,
    as_func: bool,
) -> Result<Program<'a>, &'static str> {
    let into_ast: IntoAST<'_, 'a> = IntoAST::new(&allocator, &graph);

    into_ast.try_into_ast(as_func)
}

#[cfg(test)]
mod tests {
    use super::*;

    use oxc_codegen::{CodeGenerator, CodegenOptions};

    fn convert_and_print(graph: &IRGraph, as_func: bool) -> String {
        let allocator = Allocator::default();
        let program = ir_into_ast(&allocator, graph, as_func).unwrap();
        let mut options = CodegenOptions::default();
        options.single_quote = true;
        options.minify = true;
        let printed = CodeGenerator::new()
            .with_options(options.clone())
            .build(&program);
        printed.code
    }

    #[test]
    fn single_return() {
        let mut ir = IRGraph::new();
        ir.add_constant(oxsea_ir::CompileTimeValue::Number(2.0));
        ir.add_return(1, 2);
        let code = convert_and_print(&ir, true);
        assert_eq!(code, "function f(){return 2}");
    }

    #[test]
    fn export_default() {
        let mut ir = IRGraph::new();
        ir.add_constant(oxsea_ir::CompileTimeValue::Number(2.0));
        ir.add_bind_export("default".to_string(), 2);
        let code = convert_and_print(&ir, false);
        assert_eq!(code, "export default 2;");
    }
}
