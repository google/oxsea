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

use std::usize;

use oxc_allocator::{Allocator, Vec};
use oxc_ast::ast::BindingPatternKind::BindingIdentifier;
use oxc_ast::ast::Declaration::VariableDeclaration;
use oxc_ast::ast::{
    AssignmentOperator, AssignmentTarget, BindingRestElement, Expression, FormalParameterKind,
    FunctionType, ImportOrExportKind, NumberBase, Program, SourceType, Span, Statement,
    TSThisParameter, TSTypeAnnotation, TSTypeParameterDeclaration, VariableDeclarationKind,
    WithClause,
};
use oxc_ast::AstBuilder;
use oxsea_ir::{CompileTimeValue, IRGraph};
use oxsea_traverse::{
    traverse, Add, BindExport, Compare, Constant, IRNodeKind, IfElse, LoadGlobal, PhiControl, PhiValue, Return, Visit
};

struct IntoAST<'i, 'o> {
    graph: &'i IRGraph,
    ast: AstBuilder<'o>,

    statements: Vec<'o, Statement<'o>>,
    expression_stack: Vec<'o, Expression<'o>>,
}

impl Visit for IntoAST<'_, '_> {
    fn visit_constant(&mut self, constant: &Constant) {
        match constant.value() {
            CompileTimeValue::Number(n) => self.expression_stack.push(Expression::NumericLiteral(
                self.ast
                    .alloc_numeric_literal(Span::empty(0), *n, None, NumberBase::Decimal),
            )),
            CompileTimeValue::Boolean(b) => self.expression_stack.push(Expression::BooleanLiteral(
                self.ast.alloc_boolean_literal(Span::empty(0), *b),
            )),
            _ => panic!("Not implemented - {:#?}", constant.value()),
        }
        self.walk_constant(constant);
    }

    fn visit_compare(&mut self, compare: &Compare) {
        self.visit_value(compare, compare.left_id());
        let left_expr = self.pop_expression();

        self.visit_value(compare, compare.right_id());
        let right_expr = self.pop_expression();

        self.expression_stack.push(Expression::BinaryExpression(
            self.ast.alloc_binary_expression(
                Span::empty(0),
                left_expr,
                *compare.operator(),
                right_expr,
            ),
        ));
    }

    fn visit_add(&mut self, add: &Add) {
        self.visit_value(add, add.left_id());
        let left_expr = self.pop_expression();

        self.visit_value(add, add.right_id());
        let right_expr = self.pop_expression();

        self.expression_stack.push(Expression::BinaryExpression(
            self.ast.alloc_binary_expression(
                Span::empty(0),
                left_expr,
                oxc_ast::ast::BinaryOperator::Addition,
                right_expr,
            ),
        ));
    }

    fn visit_bind_export(&mut self, bind_export: &BindExport) {
        self.visit_value(bind_export, bind_export.value_id());
        let value_expr = self.pop_expression();

        if bind_export.name() == "default" {
            self.statements.push(Statement::ExportDefaultDeclaration(
                self.ast.alloc_export_default_declaration(
                    Span::empty(0),
                    value_expr.into(),
                    self.ast
                        .module_export_name_identifier_name(Span::empty(0), bind_export.name()),
                ),
            ));
        } else {
            self.statements.push(Statement::ExportNamedDeclaration(
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
                                    self.ast.alloc_binding_identifier(
                                        Span::empty(0),
                                        bind_export.name(),
                                    ),
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
            ));
        }
        self.walk_bind_export(bind_export);
    }

    fn visit_return(&mut self, ret: &Return) {
        self.visit_value(ret, ret.value_id());
        let value_expr = self.pop_expression();

        self.statements.push(Statement::ReturnStatement(
            self.ast
                .alloc_return_statement(Span::empty(0), Some(value_expr)),
        ));
    }

    fn visit_phi_value(&mut self, phi_value: &PhiValue) {
        self.expression_stack.push(Expression::Identifier(
            self.ast
                .alloc_identifier_reference(Span::empty(0), format!("Φ{}", phi_value.node_id())),
        ));
    }

    fn visit_phi_control(&mut self, phi_control: &PhiControl) {
        self.visit_value(phi_control, phi_control.value_input_id());
        let value_expr = self.pop_expression();
        self.statements.push(Statement::ExpressionStatement(
            self.ast.alloc_expression_statement(
                Span::empty(0),
                Expression::AssignmentExpression(self.ast.alloc_assignment_expression(
                    Span::empty(0),
                    AssignmentOperator::Assign,
                    AssignmentTarget::AssignmentTargetIdentifier(
                        self.ast.alloc_identifier_reference(
                            Span::empty(0),
                            format!("Φ{}", phi_control.node_id()),
                        ),
                    ),
                    value_expr,
                )),
            ),
        ));
    }

    fn visit_load_global(&mut self, load_global: &LoadGlobal) {
        self.expression_stack.push(Expression::Identifier(
            self.ast
                .alloc_identifier_reference(Span::empty(0), load_global.name()),
        ));
    }

    fn visit_if_else(&mut self, if_else: &IfElse) {
        self.visit_value(if_else, if_else.condition_id());
        let condition_expr = self.pop_expression();

        // Collect statements created while visiting the consequent and alternate blocks.
        let stmt_pointer = self.statements.len();

        self.visit_control(if_else, if_else.consequent_id());
        // TODO: How can be do split_off and end up with the right types?
        let consequent_stmts = self.get_statement_tail(stmt_pointer);

        self.visit_control(if_else, if_else.alternate_id());
        let alternate_stmts = self.get_statement_tail(stmt_pointer);

        self.statements.push(Statement::IfStatement(
            self.ast.alloc_if_statement(
                Span::empty(0),
                condition_expr,
                Statement::BlockStatement(
                    self.ast
                        .alloc_block_statement(Span::empty(0), consequent_stmts),
                ),
                if alternate_stmts.is_empty() {
                    None
                } else {
                    Some(Statement::BlockStatement(
                        self.ast
                            .alloc_block_statement(Span::empty(0), alternate_stmts),
                    ))
                },
            ),
        ));
    }
}

impl<'i, 'o> IntoAST<'i, 'o> {
    pub fn new(allocator: &'o Allocator, graph: &'i IRGraph) -> Self {
        let ast = AstBuilder::new(allocator);
        Self {
            graph: &graph,
            ast: ast,

            statements: ast.vec(),
            expression_stack: ast.vec(),
        }
    }

    fn get_statement_tail(&mut self, tail_index: usize) -> Vec<'o, Statement<'o>> {
        if tail_index == self.statements.len() {
            self.ast.vec()
        } else {
            let tail = self.statements.split_off(tail_index);
            let mut tail_vec = self.ast.vec_with_capacity(tail.len());
            for stmt in tail {
                tail_vec.push(stmt);
            }
            return tail_vec;
        }
    }

    fn pop_expression(&mut self) -> Expression<'o> {
        if let Some(expr) = self.expression_stack.pop() {
            expr
        } else {
            Expression::Identifier(
                self.ast
                    .alloc_identifier_reference(Span::empty(0), "__empty_expression_stack__"),
            )
        }
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

    fn traverse_and_wrap(&mut self, as_func: bool) -> Result<Vec<'o, Statement<'o>>, &'static str> {
        traverse(self.graph, self);
        let stmts = self.ast.move_vec(&mut self.statements);
        if as_func {
            Ok(self.wrap_in_function_declaration(stmts))
        } else {
            Ok(stmts)
        }
    }

    pub fn try_into_ast(&mut self, as_func: bool) -> Result<Program<'o>, &'static str> {
        let stmts = self.traverse_and_wrap(as_func)?;
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
    let mut into_ast: IntoAST<'_, 'a> = IntoAST::new(&allocator, &graph);

    into_ast.try_into_ast(as_func)
}

#[cfg(test)]
mod tests {
    use super::*;

    use oxc_codegen::{CodeGenerator, CodegenOptions};
    use oxsea_ir::IR_START_ID;

    fn convert_and_print(graph: &IRGraph, as_func: bool) -> String {
        let allocator = Allocator::default();
        let program = ir_into_ast(&allocator, graph, as_func).unwrap();
        let mut options = CodegenOptions::default();
        options.single_quote = true;
        options.minify = false;
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
        assert_eq!(code, "function f() {\n\treturn 2;\n}\n");
    }

    #[test]
    fn export_default() {
        let mut ir = IRGraph::new();
        let value = ir.add_constant(oxsea_ir::CompileTimeValue::Number(2.0));
        ir.add_bind_export("default".to_string(), value);
        let code = convert_and_print(&ir, false);
        assert_eq!(code, "export default 2;\n");
    }

    #[test]
    fn if_else() {
        let mut ir = IRGraph::new();
        let a = ir.add_constant(oxsea_ir::CompileTimeValue::Number(2.0));
        let b = ir.add_constant(oxsea_ir::CompileTimeValue::Number(4.0));
        let cond = ir.add_load_global("cond".to_string());
        let consequent_id = ir.add_block();
        let alternate_id = ir.add_block();
        let phi = ir.add_phi(&[consequent_id, a, alternate_id, b]);
        ir.add_if_else(
            IR_START_ID,
            cond,
            consequent_id,
            alternate_id,
            consequent_id,
            alternate_id,
        );
        ir.add_bind_export("default".to_string(), phi);
        let code = convert_and_print(&ir, false);
        assert_eq!(
            code,
            "if (cond) {\n\tΦ7 = 2;\n} else {\n\tΦ7 = 4;\n}\nexport default Φ7;\n"
        );
    }
}
