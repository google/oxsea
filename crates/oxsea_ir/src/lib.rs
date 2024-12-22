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
pub enum IteratorKind {
    Enumerate,
    Sync,
    Async,
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
    IfElse(Option<bool>),
    Proj(usize),
    // Merge node, optionally resolved to a single branch.
    Merge(Option<usize>),
    Phi,
    GetIterator(IteratorKind),
    IteratorNext,
    Loop,
    // Inserted to resolve cycles in the graph.
    PendingResolution,
}

impl IRInstruction {
    pub fn is_control(&self) -> bool {
        match self {
            IRInstruction::End
            | IRInstruction::Start
            | IRInstruction::Unreachable
            | IRInstruction::Return
            | IRInstruction::BindExport(_)
            | IRInstruction::IfElse(_)
            | IRInstruction::Loop
            | IRInstruction::Proj(_)
            | IRInstruction::Merge(_) => true,
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

    fn get_resolved_multi_output_index(&self) -> Option<(usize, IRNodeId)> {
        match self.instruction {
            IRInstruction::IfElse(Some(value)) => {
                if value {
                    Some((0, self.inputs[0]))
                } else {
                    Some((1, self.inputs[0]))
                }
            }
            _ => None,
        }
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
            IRInstruction::IfElse(None) => {
                let condition_node = &graph.nodes[self.inputs[1]];

                if let IRInstruction::Constant(CompileTimeValue::Boolean(true)) =
                    &condition_node.instruction
                {
                    return Some(PeepholeResult::New(IRNode {
                        instruction: IRInstruction::IfElse(Some(true)),
                        inputs: self.inputs.clone(),
                        outputs: self.outputs.clone(),
                    }));
                } else if let IRInstruction::Constant(CompileTimeValue::Boolean(false)) =
                    &condition_node.instruction
                {
                    return Some(PeepholeResult::New(IRNode {
                        instruction: IRInstruction::IfElse(Some(false)),
                        inputs: self.inputs.clone(),
                        outputs: self.outputs.clone(),
                    }));
                }
            }
            IRInstruction::Merge(resolved) => {
                // TODO: If there are no phi outputs, merge this node into the predecessor.

                match resolved {
                    Some(_) => {
                        // Ignored for now.
                    }
                    None => {
                        // Check: if the branch point resolved?
                        let branch_point_id = self.inputs[0];
                        let branch_point = graph.get_node(branch_point_id);
                        if let Some((active_index, _)) =
                            branch_point.get_resolved_multi_output_index()
                        {
                            let resolved_tail_id = self.inputs[1 + active_index];
                            let node = IRNode {
                                instruction: IRInstruction::Merge(Some(active_index)),
                                inputs: vec![resolved_tail_id],
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
            IRInstruction::Proj(index) => {
                if self.inputs.len() != 1 {
                    panic!("Proj without a single input: {:#?}", self);
                }

                let prev_id = self.inputs[0];
                let prev_node = &graph.nodes[prev_id];
                if let Some((active_index, ctrl_id)) = prev_node.get_resolved_multi_output_index() {
                    if active_index == index {
                        return Some(PeepholeResult::Existing(ctrl_id));
                    } else {
                        return Some(PeepholeResult::New(IRNode {
                            instruction: IRInstruction::Unreachable,
                            inputs: vec![],
                            outputs: vec![],
                        }));
                    }
                }
            }
            _ => {}
        }
        None
    }

    pub fn is_dead_value(&self) -> bool {
        !self.instruction.is_control() && self.outputs.len() == 0
    }

    pub fn is_dead_control(&self) -> bool {
        match self.instruction {
            // Start and End are never dead.
            IRInstruction::End | IRInstruction::Start => false,
            _ => {
                // Unless the control node comes from somewhere AND goes somewhere, it's dead.
                self.instruction.is_control() && (self.inputs.len() == 0 || self.outputs.len() == 0)
            }
        }
    }

    pub fn is_dead(&self) -> bool {
        self.is_dead_control() || self.is_dead_value()
    }

    pub fn is_control(&self, graph: &IRGraph) -> bool {
        if let IRInstruction::Proj(_) = self.instruction {
            // For projection nodes, check if the input is a control node.
            return graph.get_node(self.inputs[0]).is_control(graph);
        } else {
            self.instruction().is_control()
        }
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

    pub fn get_node_mut(&mut self, id: usize) -> &mut IRNode {
        &mut self.nodes[id]
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

    pub fn add_proj(&mut self, control_id: IRNodeId, index: usize) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Proj(index),
            inputs: vec![control_id],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_if_else(&mut self, control: IRNodeId, condition: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::IfElse(None),
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

    pub fn add_get_iterator(&mut self, arr: IRNodeId, kind: IteratorKind) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::GetIterator(kind),
            inputs: vec![arr],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_iterator_next(&mut self, it_record: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::IteratorNext,
            inputs: vec![it_record],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_loop(&mut self, control: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Loop,
            inputs: vec![control],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_loop_with_repeat(&mut self, control: IRNodeId, repeat_control: IRNodeId) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::Loop,
            inputs: vec![control, repeat_control],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn add_pending_resolution(&mut self) -> IRNodeId {
        let node = IRNode {
            instruction: IRInstruction::PendingResolution,
            inputs: vec![],
            outputs: vec![],
        };
        self.add_node(node)
    }

    pub fn set_resolution(&mut self, pending_id: IRNodeId, output_graph_id: IRNodeId) {
        let pending = &mut self.nodes[pending_id];
        assert!(pending.inputs().len() == 0, "Unresolved node may not have inputs");
        let consuming_ids = pending.outputs().clone();
        pending.outputs.clear();
        println!("Setting resolution for node {:#?}: {:?}", pending, output_graph_id);

        for consuming_id in consuming_ids {
            let consuming = &mut self.nodes[consuming_id];
            let mut found = false;
            for input in consuming.inputs.iter_mut() {
                if *input == pending_id {
                    *input = output_graph_id;
                    found = true;
                }
            }
            assert!(found, "Resolution not found in consuming node");
        }
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
        if let IRInstruction::Proj(_) = node.instruction() {
            continue;
        }

        let mut input_names: Vec<String> = vec![];
        let mut output_names: Vec<String> = vec![];
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
                    "[label=\"never\", shape=hexagon, fillcolor=\"#fcc\"]"
                ));
                if node.is_dead() {
                    dot.push_str(" [style=\"filled, dashed\"]");
                } else {
                    dot.push_str(" [style=filled]");
                }
                use_default_shape = false;
            }
            IRInstruction::Return => {
                label.push_str("return");
                input_names.push("control".to_string());
                input_names.push("value".to_string());
            }
            IRInstruction::IfElse(None) => {
                label.push_str("if");
                input_names.push("control".to_string());
                input_names.push("condition".to_string());
                output_names.push("true".to_string());
                output_names.push("false".to_string());
            }
            IRInstruction::IfElse(Some(val)) => {
                label.push_str(&format!("if ({})", val));
                input_names.push("control".to_string());
                input_names.push("condition".to_string());
                output_names.push("true".to_string());
                output_names.push("false".to_string());
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
            IRInstruction::Proj(_) => {
                panic!("Proj nodes should not be rendered");
            }
            IRInstruction::PendingResolution => {
                label.push_str(&"<pending>");
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
            IRInstruction::GetIterator(kind) => {
                label.push_str(&format!("%GetIterator(kind={:?})", kind));
            }
            IRInstruction::IteratorNext => {
                label.push_str("%IteratorNext");
                output_names.push("this".to_string());
                output_names.push("done".to_string());
                output_names.push("value".to_string());
            }
            IRInstruction::Loop => {
                label.push_str("loop");
                input_names.push("start".to_string());
                input_names.push("repeat".to_string());
            }
        }

        let has_named_inputs = input_names.len() > 0;
        if has_named_inputs && input_names.len() != node.inputs().len() {
            panic!("input_attrs.len() != node.inputs().len()");
        }

        if use_default_shape {
            let is_unused = node.is_dead();
            if input_names.len() > 0 || output_names.len() > 0 {
                dot.push_str(&"[shape=record, label=\"{");
                if input_names.len() > 0 {
                    dot.push_str(&"{in|");
                    for (input_idx, input_name) in input_names.iter().enumerate() {
                        if input_idx > 0 {
                            dot.push_str("|");
                        }
                        dot.push_str(&format!("<i{}> {}", input_idx, input_name));
                    }
                    dot.push_str(&"}|");
                }
                dot.push_str(&label);
                if output_names.len() > 0 {
                    dot.push_str(&"|{out|");
                    for (output_idx, output_name) in output_names.iter().enumerate() {
                        if output_idx > 0 {
                            dot.push_str("|");
                        }
                        dot.push_str(&format!("<o{}> {}", output_idx, output_name));
                    }
                    dot.push_str(&"}");
                }
                dot.push_str(&"}\"]");
            } else {
                dot.push_str(&format!("[shape=box, label=\"{}\"]", label));
            }
            if node.is_control(graph) {
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

        fn format_input(graph: &IRGraph, input_id: IRNodeId) -> String {
            let node = graph.get_node(input_id);
            if let IRInstruction::Proj(index) = node.instruction() {
                format!("n{}:o{}", node.inputs()[0], index)
            } else {
                format!("n{}", input_id)
            }
        }

        if node.instruction == IRInstruction::Phi {
            dot.push_str(&format!(
                "  n{} -> n{}:i{} [style=dotted];\n",
                node.inputs[0], pos, 0
            ));
            for (input_idx, input) in node.inputs().iter().skip(1).enumerate() {
                dot.push_str(&format!(
                    "  {} -> n{}:i{};\n",
                    format_input(graph, *input),
                    pos,
                    input_idx + 1
                ));
            }
        } else {
            for (input_idx, input) in node.inputs().iter().enumerate() {
                let style = if node.is_control(graph) && graph.get_node(*input).is_control(graph) {
                    "[color=red]"
                } else {
                    ""
                };
                dot.push_str(&format!(
                    "  {} -> n{}:i{} {};\n",
                    format_input(graph, *input),
                    pos,
                    input_idx,
                    style
                ));
            }
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
        let branch_point = ir.add_if_else(IR_START_ID, cond);
        let consequent = ir.add_proj(branch_point, 0);
        let alternate = ir.add_proj(branch_point, 1);
        let two = ir.add_constant(Number(2.0));
        let three = ir.add_constant(Number(3.0));
        let branch_merge = ir.add_merge(branch_point, &[consequent, alternate]);
        let phi = ir.add_phi(branch_merge, &[two, three]);
        ir.add_return(branch_merge, phi);

        for i in 0..ir.len() {
            assert!(
                match ir.get_node(i).instruction {
                    IRInstruction::IfElse(None) => false,
                    IRInstruction::IfElse(Some(_)) => ir.get_node(i).is_dead_control(),
                    _ => true,
                },
                "Found alive IfElse node at {}: {:#?}",
                i,
                ir.get_node(i)
            );
            assert!(
                match ir.get_node(i).instruction {
                    IRInstruction::Proj(_) => false,
                    _ => true,
                },
                "Found output select node at {}: {:#?}",
                i,
                ir.get_node(i)
            );
        }
    }
}
