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

use oxc_ast::ast::BinaryOperator;

pub type IRNodeId = usize;

pub const IR_END_ID: IRNodeId = 0;
pub const IR_START_ID: IRNodeId = 1;

#[derive(Debug, PartialEq, Clone)]
pub enum CompileTimeValue {
    Undetermined,
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

#[derive(Debug, PartialEq)]
pub enum IRInstruction {
    End,
    Start,
    Return,
    Add,
    Compare(BinaryOperator),
    Constant(CompileTimeValue),
    BindExport(String),
    LoadGlobal(String),
    IfElse,
    Block,
    Merge,
    Phi,
}

impl IRInstruction {
    pub fn is_control(&self) -> bool {
        match self {
            IRInstruction::End
            | IRInstruction::Start
            | IRInstruction::Return
            | IRInstruction::BindExport(_)
            | IRInstruction::IfElse
            | IRInstruction::Block
            | IRInstruction::Merge => true,
            _ => false,
        }
    }

    pub fn has_named_inputs(&self) -> bool {
        match self {
            IRInstruction::Return | IRInstruction::BindExport(_) | IRInstruction::Add => true,
            _ => false,
        }
    }
}

#[derive(PartialEq)]
pub struct IRNode {
    instruction: IRInstruction,
    inputs: std::vec::Vec<IRNodeId>,
    outputs: std::vec::Vec<IRNodeId>,
}

fn calc_compile_time_binary_expression(
    left: &CompileTimeValue,
    right: &CompileTimeValue,
    op: &BinaryOperator,
) -> Option<CompileTimeValue> {
    match op {
        BinaryOperator::Addition => match (left, right) {
            (CompileTimeValue::Number(left), CompileTimeValue::Number(right)) => {
                Some(CompileTimeValue::Number(left + right))
            }
            _ => None,
        },
        BinaryOperator::Equality => match (left, right) {
            (CompileTimeValue::Number(left), CompileTimeValue::Number(right)) => {
                Some(CompileTimeValue::Boolean(left == right))
            }
            (CompileTimeValue::String(left), CompileTimeValue::String(right)) => {
                Some(CompileTimeValue::Boolean(left == right))
            }
            (CompileTimeValue::Boolean(left), CompileTimeValue::Boolean(right)) => {
                Some(CompileTimeValue::Boolean(left == right))
            }
            (
                CompileTimeValue::Null | CompileTimeValue::Undefined,
                CompileTimeValue::Null | CompileTimeValue::Undefined,
            ) => Some(CompileTimeValue::Boolean(true)),
            _ => None,
        },
        BinaryOperator::StrictEquality => match (left, right) {
            (CompileTimeValue::Number(left), CompileTimeValue::Number(right)) => {
                Some(CompileTimeValue::Boolean(left == right))
            }
            (CompileTimeValue::String(left), CompileTimeValue::String(right)) => {
                Some(CompileTimeValue::Boolean(left == right))
            }
            (CompileTimeValue::Boolean(left), CompileTimeValue::Boolean(right)) => {
                Some(CompileTimeValue::Boolean(left == right))
            }
            (CompileTimeValue::Null, CompileTimeValue::Null) => {
                Some(CompileTimeValue::Boolean(true))
            }
            (CompileTimeValue::Undefined, CompileTimeValue::Undefined) => {
                Some(CompileTimeValue::Boolean(true))
            }
            (CompileTimeValue::Null | CompileTimeValue::Undefined, _) => {
                Some(CompileTimeValue::Boolean(false))
            }
            (_, CompileTimeValue::Null | CompileTimeValue::Undefined) => {
                Some(CompileTimeValue::Boolean(false))
            }
            _ => None,
        },
        BinaryOperator::Inequality => match (left, right) {
            (CompileTimeValue::Number(left), CompileTimeValue::Number(right)) => {
                Some(CompileTimeValue::Boolean(left != right))
            }
            (CompileTimeValue::String(left), CompileTimeValue::String(right)) => {
                Some(CompileTimeValue::Boolean(left != right))
            }
            (CompileTimeValue::Boolean(left), CompileTimeValue::Boolean(right)) => {
                Some(CompileTimeValue::Boolean(left != right))
            }
            _ => None,
        },
        _ => None,
    }
}

impl IRNode {
    pub fn new(
        instruction: IRInstruction,
        inputs: std::vec::Vec<IRNodeId>,
        outputs: std::vec::Vec<IRNodeId>,
    ) -> Self {
        Self {
            instruction: instruction,
            inputs: inputs,
            outputs: outputs,
        }
    }

    fn from_compile_time_binary_expression(
        left: &CompileTimeValue,
        right: &CompileTimeValue,
        op: &BinaryOperator,
    ) -> Option<IRNode> {
        calc_compile_time_binary_expression(&left, &right, &op)
            .map(|value| IRNode::new(IRInstruction::Constant(value), vec![], vec![]))
    }

    pub fn add_input(&mut self, input: IRNodeId) {
        self.inputs.push(input);
    }

    pub fn add_output(&mut self, output: IRNodeId) {
        self.outputs.push(output);
    }

    pub fn has_output(&self, node: IRNodeId) -> bool {
        self.outputs.contains(&node)
    }

    pub fn instruction(&self) -> &IRInstruction {
        &self.instruction
    }

    pub fn inputs(&self) -> &std::vec::Vec<IRNodeId> {
        &self.inputs
    }

    pub fn outputs(&self) -> &std::vec::Vec<IRNodeId> {
        &self.outputs
    }

    pub fn to_string(&self) -> String {
        String::new()
    }

    pub fn peephole_optimize(&self, graph: &IRGraph) -> Option<IRNode> {
        match self.instruction {
            IRInstruction::Add => {
                let left_node = &graph.nodes[self.inputs[0]];
                let right_node = &graph.nodes[self.inputs[1]];

                if let (IRInstruction::Constant(left), IRInstruction::Constant(right)) =
                    (&left_node.instruction, &right_node.instruction)
                {
                    return IRNode::from_compile_time_binary_expression(
                        &left,
                        &right,
                        &BinaryOperator::Addition,
                    );
                }
            }
            IRInstruction::Compare(op) => {
                let left_node = &graph.nodes[self.inputs[0]];
                let right_node = &graph.nodes[self.inputs[1]];

                if let (IRInstruction::Constant(left), IRInstruction::Constant(right)) =
                    (&left_node.instruction, &right_node.instruction)
                {
                    return IRNode::from_compile_time_binary_expression(&left, &right, &op);
                }
            }
            _ => {}
        }
        None
    }
}

impl std::fmt::Debug for IRNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:#?} {{{:#?}}} => {{{:#?}}}",
            self.instruction, self.inputs, self.outputs
        )
    }
}

pub struct IRGraph {
    nodes: std::vec::Vec<IRNode>,
}

impl std::fmt::Debug for IRGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pad = 2;
        for (pos, node) in self.nodes.iter().enumerate() {
            write!(f, "#{:0pad$}: {:#?}\n", pos, node, pad = pad)?;
        }
        std::fmt::Result::Ok(())
    }
}

impl IRGraph {
    pub fn new() -> Self {
        Self {
            nodes: vec![
                IRNode {
                    instruction: IRInstruction::End,
                    inputs: vec![],
                    outputs: vec![],
                },
                IRNode {
                    instruction: IRInstruction::Start,
                    inputs: vec![],
                    outputs: vec![],
                },
            ],
        }
    }

    pub fn add_edge(&mut self, from: IRNodeId, to: IRNodeId) {
        if !self.nodes[from].has_output(to) {
            self.nodes[from].add_output(to);
            self.nodes[to].add_input(from);
        }
    }

    pub fn get_node(&self, id: IRNodeId) -> &IRNode {
        &self.nodes[id]
    }

    pub fn end_node(&self) -> &IRNode {
        &self.nodes[IR_END_ID]
    }

    fn add_node(&mut self, original_node: IRNode) -> IRNodeId {
        let node = original_node
            .peephole_optimize(self)
            .unwrap_or(original_node);
        let index = self.nodes.len();
        self.nodes.push(node);

        let outputs = self.nodes[index].outputs.clone();
        for output in outputs {
            self.nodes[output].add_input(index);
        }

        let inputs = self.nodes[index].inputs.clone();
        for input in inputs {
            self.nodes[input].add_output(index);
        }
        index
    }

    pub fn add_constant(&mut self, value: CompileTimeValue) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Constant(value),
            inputs: vec![],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_add(&mut self, left: IRNodeId, right: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Add,
            inputs: vec![left, right],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_compare(&mut self, left: IRNodeId, right: IRNodeId, op: BinaryOperator) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Compare(op),
            inputs: vec![left, right],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_return(&mut self, control: IRNodeId, value: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Return,
            inputs: vec![control, value],
            outputs: vec![IR_END_ID],
        };
        self.add_node(node)
    }

    pub fn add_if_else(&mut self, control: IRNodeId, condition: IRNodeId) -> (IRNodeId, IRNodeId, IRNodeId) {
        let node = IRNode {
            instruction: IRInstruction::IfElse,
            inputs: vec![control, condition],
            outputs: vec![],
        };
        let if_else_id = self.add_node(node);
        let consequent = IRNode {
            instruction: IRInstruction::Block,
            inputs: vec![if_else_id],
            outputs: vec![],
        };
        let consequent_id = self.add_node(consequent);
        let alternate = IRNode {
            instruction: IRInstruction::Block,
            inputs: vec![if_else_id],
            outputs: vec![],
        };
        let alternate_id = self.add_node(alternate);

        return (if_else_id, consequent_id, alternate_id);
    }

    pub fn add_merge(&mut self, controls: &[IRNodeId]) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Merge,
            inputs: Vec::from(controls),
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_bind_export(&mut self, name: String, value: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::BindExport(name),
            inputs: vec![value],
            outputs: vec![IR_END_ID],
        };
        self.add_node(node)
    }

    pub fn add_load_global(&mut self, name: String) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::LoadGlobal(name),
            inputs: vec![],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_phi(&mut self, alternate_values: &[IRNodeId]) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Phi,
            inputs: Vec::from(alternate_values),
            outputs: vec![],
        };
        self.add_node(node)
    }
}

pub fn ir_to_dot(graph: &IRGraph) -> String {
    let mut dot = String::new();
    dot.push_str(
        "\
        digraph {\n\
        \x20 fontname=\"Helvetica,Arial,sans-serif\";\n\
        \x20 node [fontname=\"Helvetica,Arial,sans-serif\"];\n\
        \x20 edge [fontname=\"Helvetica,Arial,sans-serif\"];\n\
        ",
    );
    for (pos, node) in graph.nodes.iter().enumerate() {
        let mut input_names: Vec<String> = vec![];
        let mut use_default_shape = true;
        let mut label = String::new();

        dot.push_str(&format!("  n{} ", pos));
        match &node.instruction {
            IRInstruction::End => {
                dot.push_str(&format!("[label=\"\", shape=circle, style=filled, fillcolor=\"#fff\", height=0.3, width=0.3, penwidth=5]"));
                use_default_shape = false;
            }
            IRInstruction::Start => {
                dot.push_str(&format!(
                    "[shape=point, style=filled, fillcolor=\"#000\", height=0.25, width=0.25]"
                ));
                use_default_shape = false;
            }
            IRInstruction::Return => {
                label.push_str("return");
                input_names.push("control".to_string());
                input_names.push("value".to_string());
            }
            IRInstruction::IfElse => {
                label.push_str("if");
                input_names.push("control".to_string());
                input_names.push("condition".to_string());
            }
            IRInstruction::Merge => {
                label.push_str("merge");
            }
            IRInstruction::Block => {
                label.push_str("block");
            }
            IRInstruction::Phi => {
                label.push_str("Φ");
                for (input_idx, _) in node.inputs().iter().enumerate() {
                    if input_idx % 2 == 0 {
                        input_names.push(format!("c{}", input_idx / 2));
                    } else {
                        input_names.push(format!("v{}", input_idx / 2));
                    }
                }
            }
            IRInstruction::Add => {
                label.push_str("+");
                input_names.push("left".to_string());
                input_names.push("right".to_string());
            }
            IRInstruction::Compare(op) => {
                label.push_str(op.as_str());
                input_names.push("left".to_string());
                input_names.push("right".to_string());
            }
            IRInstruction::Constant(value) => {
                label.push_str("= ");
                match value {
                    CompileTimeValue::Number(n) => {
                        label.push_str(&format!("{:?}", n));
                    }
                    CompileTimeValue::String(s) => {
                        label.push_str(&format!("{:#?}", s));
                    }
                    CompileTimeValue::Boolean(b) => {
                        label.push_str(&format!("{:?}", b));
                    }
                    CompileTimeValue::Null => {
                        label.push_str("null");
                    }
                    CompileTimeValue::Undefined => {
                        label.push_str("undefined");
                    }
                    CompileTimeValue::Undetermined => {
                        label.push_str("<undetermined>");
                    }
                }
            }
            IRInstruction::BindExport(name) => {
                label.push_str(&format!("export '{}'", name));
                input_names.push("value".to_string());
            }
            IRInstruction::LoadGlobal(name) => {
                label.push_str(&format!("global '{}'", name));
            }
        }

        let has_named_inputs = input_names.len() > 0;
        if has_named_inputs && input_names.len() != node.inputs().len() {
            panic!("input_attrs.len() != node.inputs().len()");
        }

        if use_default_shape {
            let is_unused = node.instruction != IRInstruction::End && node.outputs().len() == 0;
            let has_named_inputs = input_names.len() > 0;
            // n7 [shape=record, label="{{<i0> control|<i1> value}|export 'default'}", style=filled, fillcolor="#ffffaa"];
            if has_named_inputs {
                dot.push_str(&"[shape=record, label=\"{{");
                for (input_idx, input_name) in input_names.iter().enumerate() {
                    if input_idx > 0 {
                        dot.push_str("|");
                    }
                    dot.push_str(&format!("<i{}> {}", input_idx, input_name));
                }
                dot.push_str(&format!("}}|{}}}\"]", label));
            } else {
                dot.push_str(&format!("[shape=box, label=\"{}\"]", label));
            }
            if node.instruction.is_control() {
                if is_unused {
                    dot.push_str(" [style=\"filled, dashed\", fillcolor=\"#ffffaa\"]");
                } else {
                    dot.push_str(" [style=filled, fillcolor=\"#ffffaa\"]");
                }
            } else {
                if node.instruction != IRInstruction::End && node.outputs().len() == 0 {
                    dot.push_str(" [style=\"rounded, dashed\"]");
                } else {
                    dot.push_str(" [style=rounded]");
                }
            }
        }

        dot.push_str(";\n");
        for (input_idx, input) in node.inputs().iter().enumerate() {
            dot.push_str(&format!("  n{} -> n{}:i{};\n", input, pos, input_idx));
        }
    }
    dot.push_str("}\n");
    dot
}

#[cfg(test)]
mod tests {
    use super::*;
    use CompileTimeValue::*;
    use IRInstruction::*;

    #[test]
    fn folds_numerical_constant_binop() {
        let mut ir = IRGraph::new();
        let left = ir.add_constant(Number(1.0));
        let right = ir.add_constant(Number(2.0));
        let add = ir.add_add(left, right);
        ir.add_return(IR_START_ID, add);
        assert_eq!(
            ir.get_node(4),
            &IRNode {
                instruction: Constant(Number(3.0)),
                inputs: vec![],
                outputs: vec![5],
            }
        );
    }
}