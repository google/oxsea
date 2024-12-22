use oxsea_ir::{IRGraph, IRInstruction, IRNodeId, IR_END_ID, IR_INVALID_ID, IR_START_ID};

use crate::{
    sort::reverse_topological_sort, Add, BindExport, Compare, Constant, GetIterator, IfElse, IteratorNext, LoadGlobal, Loop, Merge, Phi, Proj, Return
};

#[derive(Clone)]
pub enum IRNodeMapping {
    Pending,
    Placeholder(IRNodeId, std::vec::Vec<IRNodeId>),
    Resolved(IRNodeId),
}

pub struct OutputGraph {
    pub ir: IRGraph,
    input_id_to_output_id: std::vec::Vec<IRNodeId>,
}

impl OutputGraph {
    fn new(input_ir: &IRGraph) -> Self {
        let mut input_id_to_output_id = vec![IR_INVALID_ID; input_ir.len()];
        input_id_to_output_id[IR_START_ID] = IR_START_ID;
        input_id_to_output_id[IR_END_ID] = IR_END_ID;
        Self {
            ir: IRGraph::new(),
            input_id_to_output_id,
        }
    }

    pub fn map_node_id(&mut self, input_graph_id: IRNodeId) -> IRNodeId {
        let id = self.input_id_to_output_id[input_graph_id];
        if id == IR_INVALID_ID {
            let pending_id = self.ir.add_pending_resolution();
            self.input_id_to_output_id[input_graph_id] = pending_id;
            pending_id
        } else {
            id
        }
    }

    pub fn map_node_ids(&mut self, input_graph_ids: &[IRNodeId]) -> std::vec::Vec<IRNodeId> {
        input_graph_ids
            .iter()
            .map(|id| self.map_node_id(*id))
            .collect()
    }

    fn set_resolved_node_id(&mut self, input_graph_id: IRNodeId, output_graph_id: IRNodeId) {
        let existing = &self.input_id_to_output_id[input_graph_id];
        if *existing == IR_INVALID_ID {
            self.input_id_to_output_id[input_graph_id] = output_graph_id;
        } else {
            self.ir.set_resolution(*existing, output_graph_id);
        }
    }
}

macro_rules! transform_node_id {
    ($out:ident, $node:ident, $field:ident) => {
        let $field = $out.map_node_id($node.$field());
    };
}

macro_rules! transform_node_ids {
    ($out:ident, $node:ident, $field:ident) => {
        let $field = $out.map_node_ids(&$node.$field());
    };
}

pub trait Transform {
    fn visit_constant(&mut self, constant: &Constant, out: &mut OutputGraph) -> IRNodeId {
        self.transform_constant(constant, out)
            .unwrap_or_else(|| out.ir.add_constant(constant.value().clone()))
    }

    fn transform_constant(
        &mut self,
        _constant: &Constant,
        _out: &mut OutputGraph,
    ) -> Option<IRNodeId> {
        None
    }

    fn visit_return(&mut self, ret: &Return, out: &mut OutputGraph) -> IRNodeId {
        self.transform_return(ret, out).unwrap_or_else(|| {
            transform_node_id!(out, ret, control_id);
            transform_node_id!(out, ret, value_id);
            out.ir.add_return(control_id, value_id)
        })
    }

    fn transform_return(&mut self, _ret: &Return, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_bind_export(&mut self, bind_export: &BindExport, out: &mut OutputGraph) -> IRNodeId {
        self.transform_bind_export(bind_export, out)
            .unwrap_or_else(|| {
                transform_node_id!(out, bind_export, control_id);
                transform_node_id!(out, bind_export, value_id);
                out.ir
                    .add_bind_export(control_id, bind_export.name().to_string(), value_id)
            })
    }

    fn transform_bind_export(
        &mut self,
        _bind_export: &BindExport,
        _out: &mut OutputGraph,
    ) -> Option<IRNodeId> {
        None
    }

    fn visit_load_global(&mut self, load_global: &LoadGlobal, out: &mut OutputGraph) -> IRNodeId {
        self.transform_load_global(load_global, out)
            .unwrap_or_else(|| out.ir.add_load_global(load_global.name().to_string()))
    }

    fn transform_load_global(
        &mut self,
        _load_global: &LoadGlobal,
        _out: &mut OutputGraph,
    ) -> Option<IRNodeId> {
        None
    }

    fn visit_compare(&mut self, compare: &Compare, out: &mut OutputGraph) -> IRNodeId {
        self.transform_compare(compare, out).unwrap_or_else(|| {
            transform_node_id!(out, compare, left_id);
            transform_node_id!(out, compare, right_id);
            out.ir.add_compare(left_id, right_id, *compare.operator())
        })
    }

    fn transform_compare(
        &mut self,
        _compare: &Compare,
        _out: &mut OutputGraph,
    ) -> Option<IRNodeId> {
        None
    }

    fn visit_add(&mut self, add: &Add, out: &mut OutputGraph) -> IRNodeId {
        self.transform_add(add, out).unwrap_or_else(|| {
            transform_node_id!(out, add, left_id);
            transform_node_id!(out, add, right_id);
            out.ir.add_add(left_id, right_id)
        })
    }

    fn transform_add(&mut self, _add: &Add, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_if_else(&mut self, if_else: &IfElse, out: &mut OutputGraph) -> IRNodeId {
        self.transform_if_else(if_else, out).unwrap_or_else(|| {
            transform_node_id!(out, if_else, control_id);
            transform_node_id!(out, if_else, condition_id);
            out.ir.add_if_else(control_id, condition_id)
        })
    }

    fn transform_if_else(&mut self, _if_else: &IfElse, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_proj(&mut self, proj: &Proj, out: &mut OutputGraph) -> IRNodeId {
        self.transform_proj(proj, out).unwrap_or_else(|| {
            transform_node_id!(out, proj, control_id);
            out.ir.add_proj(control_id, proj.index())
        })
    }

    fn transform_proj(&mut self, _proj: &Proj, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_merge(&mut self, merge: &Merge, out: &mut OutputGraph) -> IRNodeId {
        self.transform_merge(merge, out).unwrap_or_else(|| {
            transform_node_id!(out, merge, branch_point_id);
            match merge.resolved_index() {
                Some(value_index) => {
                    if merge.has_phi_outputs() {
                        out.ir.add_resolved_merge(branch_point_id, *value_index)
                    } else {
                        branch_point_id
                    }
                }
                None => {
                    transform_node_ids!(out, merge, merge_input_ids);
                    out.ir.add_merge(branch_point_id, &merge_input_ids)
                }
            }
        })
    }

    fn transform_merge(&mut self, _merge: &Merge, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_phi(&mut self, phi: &Phi, out: &mut OutputGraph) -> IRNodeId {
        self.transform_phi(phi, out).unwrap_or_else(|| {
            transform_node_id!(out, phi, control_id);
            transform_node_ids!(out, phi, value_ids);
            out.ir.add_phi(control_id, &value_ids)
        })
    }

    fn transform_phi(&mut self, _phi: &Phi, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_loop(&mut self, r#loop: &Loop, out: &mut OutputGraph) -> IRNodeId {
        self.transform_loop(r#loop, out).unwrap_or_else(|| {
            transform_node_id!(out, r#loop, control_id);
            transform_node_id!(out, r#loop, repeat_control_id);
            out.ir.add_loop_with_repeat(control_id, repeat_control_id)
        })
    }

    fn transform_loop(&mut self, _loop: &Loop, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_get_iterator(&mut self, get_iterator: &GetIterator, out: &mut OutputGraph) -> IRNodeId {
        self.transform_get_iterator(get_iterator, out).unwrap_or_else(|| {
            transform_node_id!(out, get_iterator, value_id);
            out.ir.add_get_iterator(value_id, get_iterator.kind().clone())
        })
    }

    fn transform_get_iterator(&mut self, _get_iterator: &GetIterator, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_iterator_next(&mut self, iterator_next: &IteratorNext, out: &mut OutputGraph) -> IRNodeId {
        self.transform_iterator_next(iterator_next, out).unwrap_or_else(|| {
            transform_node_id!(out, iterator_next, value_id);
            out.ir.add_iterator_next(value_id)
        })
    }

    fn transform_iterator_next(&mut self, _iterator_next: &IteratorNext, _out: &mut OutputGraph) -> Option<IRNodeId> {
        None
    }
}

pub fn transform<T>(ir: &IRGraph, transform: &mut T) -> IRGraph
where
    T: Transform,
{
    let order = reverse_topological_sort(ir);
    let context = crate::Context::new(ir);

    let mut out = OutputGraph::new(ir);

    for node_id in order {
        if node_id == IR_START_ID || node_id == IR_END_ID {
            continue;
        }

        let node = ir.get_node(node_id);
        if node.is_dead_control() {
            continue;
        }

        let new_node_id = match node.instruction() {
            IRInstruction::Constant(value) => transform.visit_constant(
                &Constant {
                    ctx: &context,
                    node_id,
                    node,
                    value: value,
                },
                &mut out,
            ),
            IRInstruction::Return => transform.visit_return(
                &Return {
                    ctx: &context,
                    node_id,
                    node,
                },
                &mut out,
            ),
            IRInstruction::BindExport(name) => transform.visit_bind_export(
                &BindExport {
                    ctx: &context,
                    node_id,
                    node,
                    name,
                },
                &mut out,
            ),
            IRInstruction::LoadGlobal(name) => transform.visit_load_global(
                &LoadGlobal {
                    ctx: &context,
                    node_id,
                    node,
                    name,
                },
                &mut out,
            ),
            IRInstruction::Compare(op) => transform.visit_compare(
                &Compare {
                    ctx: &context,
                    node_id,
                    node,
                    operator: op,
                },
                &mut out,
            ),
            IRInstruction::Add => transform.visit_add(
                &Add {
                    ctx: &context,
                    node_id,
                    node,
                },
                &mut out,
            ),
            IRInstruction::IfElse(None) => transform.visit_if_else(
                &IfElse {
                    ctx: &context,
                    node_id,
                    node,

                    resolved: &None,
                },
                &mut out,
            ),
            IRInstruction::Proj(index) => transform.visit_proj(
                &Proj {
                    ctx: &context,
                    node_id,
                    node,

                    index: *index,
                },
                &mut out,
            ),
            IRInstruction::Merge(resolved_index) => transform.visit_merge(
                &Merge {
                    ctx: &context,
                    node_id,
                    node,

                    merge_input_index: 0,
                    resolved_index,
                },
                &mut out,
            ),
            IRInstruction::Phi => transform.visit_phi(
                &Phi {
                    ctx: &context,
                    node_id,
                    node,
                },
                &mut out,
            ),
            IRInstruction::Loop => transform.visit_loop(
                &Loop {
                    ctx: &context,
                    node_id,
                    node,
                },
                &mut out,
            ),
            IRInstruction::GetIterator(kind) => transform.visit_get_iterator(
                &GetIterator {
                    ctx: &context,
                    node_id,
                    node,

                    kind,
                },
                &mut out,
            ),
            IRInstruction::IteratorNext => transform.visit_iterator_next(
                &IteratorNext {
                    ctx: &context,
                    node_id,
                    node,
                },
                &mut out,
            ),
            _ => {
                panic!("Unhandled instruction: {:?}", node.instruction());
            }
        };
        out.set_resolved_node_id(node_id, new_node_id);
    }

    out.ir
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxsea_ir::CompileTimeValue::*;
    use oxsea_ir::IR_START_ID;

    struct CopyTransform {}

    impl Transform for CopyTransform {}

    #[test]
    fn copy_add_with_constants() {
        let mut ir = IRGraph::new();
        let left = ir.add_constant(Number(1.0));
        let right = ir.add_constant(Number(2.0));
        let add = ir.add_add(left, right);
        ir.add_return(IR_START_ID, add);
        let mut copy_transform = CopyTransform {};
        let out = transform(&ir, &mut copy_transform);
        assert!(out.len() == 4);
    }
}
