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
pub const IR_INVALID_ID: IRNodeId = std::u32::MAX as usize;

#[derive(Debug, PartialEq, Clone)]
pub enum CompileTimeValue {
    Undetermined,
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IRInstruction {
    End,
    Start,
    Unreachable,
    Return,
    Add,
    Compare(BinaryOperator),
    Constant(CompileTimeValue),
    BindExport(String),
    LoadGlobal(String),
    IfElse,
    Block(usize),
    // Merge node, optionally resolved to a single branch.
    Merge(Option<usize>),
    Phi,
}

impl IRInstruction {
    pub fn is_control(&self) -> bool {
        match self {
            IRInstruction::End
            | IRInstruction::Start
            | IRInstruction::Unreachable
            | IRInstruction::Return
            | IRInstruction::BindExport(_)
            | IRInstruction::IfElse
            | IRInstruction::Block(_)
            | IRInstruction::Merge(_) => true,
            _ => false,
        }
    }

    pub fn has_named_inputs(&self) -> bool {
        match self {
            IRInstruction::Return | IRInstruction::BindExport(_) | IRInstruction::Add => true,
            _ => false,
        }
    }

    pub fn is_unreachable(&self) -> bool {
        match self {
            IRInstruction::Unreachable => true,
            _ => false,
        }
    }

    fn is_linear_control_flow(&self) -> bool {
        match self {
            IRInstruction::Start | IRInstruction::Block(_) | IRInstruction::Merge(_) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct IRNode {
    instruction: IRInstruction,
    inputs: std::vec::Vec<IRNodeId>,
    outputs: std::vec::Vec<IRNodeId>,
}

pub enum PeepholeResult {
    Existing(IRNodeId),
    New(IRNode),
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

    fn remove_branch(
        &self,
        graph: &mut IRGraph,
        control_input_id: IRNodeId,
        branch_id: IRNodeId,
    ) -> Option<PeepholeResult> {
        graph.add_linear_block(&[control_input_id], &[branch_id]);

        Some(PeepholeResult::Existing(branch_id))
    }

    pub fn peephole_optimize(&self, graph: &mut IRGraph) -> Option<PeepholeResult> {
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
                    )
                    .map(|node| PeepholeResult::New(node));
                }
            }
            IRInstruction::Compare(op) => {
                let left_node = &graph.nodes[self.inputs[0]];
                let right_node = &graph.nodes[self.inputs[1]];

                if let (IRInstruction::Constant(left), IRInstruction::Constant(right)) =
                    (&left_node.instruction, &right_node.instruction)
                {
                    return IRNode::from_compile_time_binary_expression(&left, &right, &op)
                        .map(|node| PeepholeResult::New(node));
                }
            }
            IRInstruction::IfElse => {
                let control_input_id = self.inputs[0];
                let condition_node = &graph.nodes[self.inputs[1]];

                if self.outputs.len() < 2 {
                    return None;
                }

                if let IRInstruction::Constant(CompileTimeValue::Boolean(true)) =
                    &condition_node.instruction
                {
                    let branch_id = self.outputs[0];
                    return self.remove_branch(graph, control_input_id, branch_id);
                } else if let IRInstruction::Constant(CompileTimeValue::Boolean(false)) =
                    &condition_node.instruction
                {
                    // Optimize to the alternate branch, if one exists.
                    let branch_id = self.outputs[1];
                    return self.remove_branch(graph, control_input_id, branch_id);
                }
            }
            IRInstruction::Merge(resolved) => {
                // TODO: If there are no phi outputs, merge this node into the predecessor.

                match resolved {
                    Some(_) => {
                        // Ignored for now.
                    }
                    None => {
                        let branch_point_id = self.inputs[0];
                        let merging_ids = &self.inputs[1..];

                        if let Some(resolved_idx) =
                            merging_ids.iter().position(|id| *id == branch_point_id)
                        {
                            let node = IRNode {
                                instruction: IRInstruction::Merge(Some(resolved_idx)),
                                inputs: vec![branch_point_id],
                                outputs: self.outputs.clone(),
                            };
                            return Some(PeepholeResult::New(node));
                        }
                    }
                }
            }
            IRInstruction::Phi => {
                let merge_node = graph.get_node(self.inputs[0]);

                if let IRInstruction::Merge(Some(value_idx)) = merge_node.instruction() {
                    let value_id = self.inputs()[value_idx + 1];
                    return Some(PeepholeResult::Existing(value_id));
                }
            }
            IRInstruction::Block(_) => {
                if self.inputs.len() > 1 {
                    panic!("Block with multiple inputs");
                }

                if let Some(prev_id) = self.inputs.first() {
                    // let prev_id = *prev_id;
                    let prev_node = &graph.nodes[*prev_id];
                    if prev_node.instruction.is_linear_control_flow() {
                        // Just add this node's outputs to the existing flow.
                        let outputs = self.outputs.clone();
                        for output in outputs {
                            graph.add_edge(*prev_id, output);
                        }
                        return Some(PeepholeResult::Existing(*prev_id));
                    }
                }
            }
            _ => {}
        }
        None
    }

    fn set_output(&mut self, proj_index: usize, proj_id: usize) {
        if proj_index >= self.outputs.len() {
            self.outputs.resize(proj_index + 1, IR_INVALID_ID);
        }
        self.outputs[proj_index] = proj_id;
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

    pub fn len(&self) -> usize {
        return self.nodes.len();
    }

    pub fn add_edge(&mut self, from: IRNodeId, to: IRNodeId) {
        if !self.nodes[from].has_output(to) {
            self.nodes[from].add_output(to);
            self.nodes[to].add_input(from);
        }
    }

    pub fn replace_control(&mut self, node_id: IRNodeId, replacement_id: IRNodeId) {
        let node = &self.nodes[node_id];
        let inputs = node.inputs.clone();
        let outputs = node.outputs.clone();
        self.nodes[node_id].inputs.clear();
        self.nodes[node_id].outputs.clear();
        for output in &outputs {
            let target = &mut self.nodes[*output];
            let mut found = false;
            for input in target.inputs.iter_mut() {
                if *input == node_id {
                    *input = replacement_id;
                    found = true;
                }
            }
            if !found {
                target.add_input(replacement_id);
            }
        }
        for input in &inputs {
            let target = &mut self.nodes[*input];
            let mut found = false;
            for output in target.outputs.iter_mut() {
                if *output == node_id {
                    *output = replacement_id;
                    found = true;
                }
            }
            if !found {
                target.add_output(replacement_id);
            }
        }

        for output in outputs {
            self.nodes[replacement_id].add_output(output);
        }
        for input in inputs {
            self.nodes[replacement_id].add_input(input);
        }
    }

    pub fn get_node(&self, id: IRNodeId) -> &IRNode {
        &self.nodes[id]
    }

    pub fn end_node(&self) -> &IRNode {
        &self.nodes[IR_END_ID]
    }

    fn add_node(&mut self, original_node: IRNode) -> IRNodeId {
        let optimization = original_node.peephole_optimize(self);

        let node = match optimization {
            Some(PeepholeResult::New(node)) => node,
            Some(PeepholeResult::Existing(id)) => return id,
            None => original_node,
        };

        let index = self.nodes.len();
        self.nodes.push(node);

        let outputs = self.nodes[index].outputs.clone();
        for output in outputs {
            if output != IR_INVALID_ID {
                self.nodes[output].add_input(index);
            }
        }

        let inputs = self.nodes[index].inputs.clone();
        for input in inputs {
            if input != IR_INVALID_ID {
                self.nodes[input].add_output(index);
            }
        }
        index
    }

    pub fn add_unreachable(&mut self) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Unreachable,
            inputs: vec![],
            outputs: vec![],
        };
        self.add_node(node)
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

    pub fn add_block(&mut self, control_id: IRNodeId, index: usize) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Block(index),
            inputs: vec![control_id],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_unlinked_block(&mut self, index: usize) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Block(index),
            inputs: vec![],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn link_block(&mut self, target_id: IRNodeId, proj_id: IRNodeId) {
        let proj = &mut self.nodes[proj_id];
        let proj_index = match proj.instruction() {
            IRInstruction::Block(index) => *index,
            _ => panic!("Expected block"),
        };
        proj.inputs = vec![target_id];

        let target = &mut self.nodes[target_id];
        target.set_output(proj_index, proj_id);
    }

    pub fn add_linear_block(&mut self, inputs: &[IRNodeId], outputs: &[IRNodeId]) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Block(0),
            inputs: Vec::from(inputs),
            outputs: Vec::from(outputs),
        };
        self.add_node(node)
    }

    pub fn add_if_else(&mut self, control: IRNodeId, condition: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::IfElse,
            inputs: vec![control, condition],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_merge(&mut self, branch_point: IRNodeId, controls: &[IRNodeId]) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Merge(None),
            inputs: Vec::from([&[branch_point], controls].concat()),
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_resolved_merge(&mut self, branch_point: IRNodeId, value_index: usize) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Merge(Some(value_index)),
            inputs: vec![branch_point],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_bind_export(
        &mut self,
        control: IRNodeId,
        name: String,
        value: IRNodeId,
    ) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::BindExport(name),
            inputs: vec![control, value],
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

    pub fn add_phi(&mut self, control_id: IRNodeId, alternate_values: &[IRNodeId]) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Phi,
            inputs: Vec::from([&[control_id], alternate_values].concat()),
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
            IRInstruction::Unreachable => {
                dot.push_str(&format!(
                    "[label=\"never\", shape=hexagon, style=filled, fillcolor=\"#fcc\"]"
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
            IRInstruction::Merge(resolved) => {
                if let Some(resolved) = resolved {
                    label.push_str(&format!("merge = {}", resolved));
                    dot.push_str(&format!("[fontcolor=gray, color=gray]"));
                } else {
                    label.push_str("merge");
                    for (input_idx, _) in node.inputs().iter().enumerate() {
                        if input_idx == 0 {
                            input_names.push(format!("br"));
                        } else {
                            input_names.push(format!("alt[{}]", input_idx - 1));
                        }
                    }
                }
            }
            IRInstruction::Block(index) => {
                label.push_str(&format!("outputs[{}]", index));
            }
            IRInstruction::Phi => {
                label.push_str("Φ");
                for (input_idx, _) in node.inputs().iter().enumerate() {
                    if input_idx == 0 {
                        input_names.push(format!("c"));
                    } else {
                        input_names.push(format!("v{}", input_idx - 1));
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
                input_names.push("control".to_string());
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

    #[test]
    fn removes_dead_if_branch() {
        let mut ir = IRGraph::new();
        let cond = ir.add_constant(Boolean(false));
        let consequent = ir.add_unlinked_block(0);
        let alternate = ir.add_unlinked_block(1);
        let two = ir.add_constant(Number(2.0));
        let three = ir.add_constant(Number(3.0));
        let branch_point = ir.add_if_else(IR_START_ID, cond);
        let branch_merge = ir.add_merge(branch_point, &[consequent, alternate]);
        ir.link_block(branch_point, consequent);
        ir.link_block(branch_point, alternate);
        let phi = ir.add_phi(branch_merge, &[two, three]);
        ir.add_return(branch_merge, phi);

        for i in 0..ir.len() {
            assert!(ir.get_node(i).instruction != IRInstruction::IfElse);
        }
    }
}
