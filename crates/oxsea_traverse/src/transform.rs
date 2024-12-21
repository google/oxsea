use oxsea_ir::{IRGraph, IRInstruction, IRNodeId, IR_END_ID, IR_INVALID_ID, IR_START_ID};

use crate::{
    sort::reverse_topological_sort, Add, BindExport, Compare, Constant, IfElse, LoadGlobal, Merge,
    Phi, Proj, Return,
};

pub trait Transform {
    fn visit_constant(&mut self, constant: &Constant, out: &mut IRGraph) -> IRNodeId {
        self.transform_constant(constant, out)
            .unwrap_or_else(|| out.add_constant(constant.value().clone()))
    }

    fn transform_constant(&mut self, _constant: &Constant, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_return(&mut self, ret: &Return, out: &mut IRGraph) -> IRNodeId {
        self.transform_return(ret, out)
            .unwrap_or_else(|| out.add_return(ret.control_id(), ret.value_id()))
    }

    fn transform_return(&mut self, _ret: &Return, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_bind_export(&mut self, bind_export: &BindExport, out: &mut IRGraph) -> IRNodeId {
        self.transform_bind_export(bind_export, out)
            .unwrap_or_else(|| {
                out.add_bind_export(
                    bind_export.control_id(),
                    bind_export.name().to_string(),
                    bind_export.value_id(),
                )
            })
    }

    fn transform_bind_export(
        &mut self,
        _bind_export: &BindExport,
        _out: &mut IRGraph,
    ) -> Option<IRNodeId> {
        None
    }

    fn visit_load_global(&mut self, load_global: &LoadGlobal, out: &mut IRGraph) -> IRNodeId {
        self.transform_load_global(load_global, out)
            .unwrap_or_else(|| out.add_load_global(load_global.name().to_string()))
    }

    fn transform_load_global(
        &mut self,
        _load_global: &LoadGlobal,
        _out: &mut IRGraph,
    ) -> Option<IRNodeId> {
        None
    }

    fn visit_compare(&mut self, compare: &Compare, out: &mut IRGraph) -> IRNodeId {
        self.transform_compare(compare, out).unwrap_or_else(|| {
            out.add_compare(compare.left_id(), compare.right_id(), *compare.operator())
        })
    }

    fn transform_compare(&mut self, _compare: &Compare, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_add(&mut self, add: &Add, out: &mut IRGraph) -> IRNodeId {
        self.transform_add(add, out)
            .unwrap_or_else(|| out.add_add(add.left_id(), add.right_id()))
    }

    fn transform_add(&mut self, _add: &Add, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_if_else(&mut self, if_else: &IfElse, out: &mut IRGraph) -> IRNodeId {
        self.transform_if_else(if_else, out)
            .unwrap_or_else(|| out.add_if_else(if_else.control_id(), if_else.condition_id()))
    }

    fn transform_if_else(&mut self, _if_else: &IfElse, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_proj(&mut self, proj: &Proj, out: &mut IRGraph) -> IRNodeId {
        self.transform_proj(proj, out)
            .unwrap_or_else(|| out.add_proj(proj.control_id(), proj.index()))
    }

    fn transform_proj(&mut self, _proj: &Proj, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_merge(&mut self, merge: &Merge, out: &mut IRGraph) -> IRNodeId {
        self.transform_merge(merge, out)
            .unwrap_or_else(|| match merge.resolved_index() {
                Some(value_index) => {
                    if merge.has_phi_outputs() {
                        out.add_resolved_merge(merge.branch_point_id(), *value_index)
                    } else {
                        merge.branch_point_id()
                    }
                }
                None => out.add_merge(merge.branch_point_id(), &merge.merge_input_ids()),
            })
    }

    fn transform_merge(&mut self, _merge: &Merge, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_phi(&mut self, phi: &Phi, out: &mut IRGraph) -> IRNodeId {
        self.transform_phi(phi, out)
            .unwrap_or_else(|| out.add_phi(phi.control_id(), &phi.value_ids()))
    }

    fn transform_phi(&mut self, _phi: &Phi, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }
}

pub fn transform<T>(ir: &IRGraph, transform: &mut T) -> IRGraph
where
    T: Transform,
{
    let order = reverse_topological_sort(ir);
    let mut context = crate::Context::new_with_ids(ir);

    let mut out = IRGraph::new();

    for node_id in order {
        if node_id == IR_START_ID || node_id == IR_END_ID {
            continue;
        }

        let node = ir.get_node(node_id);
        if node.is_dead_control() {
            context.ids[node_id] = IR_INVALID_ID;
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
            _ => {
                panic!("Unhandled instruction: {:?}", node.instruction());
            }
        };
        context.ids[node_id] = new_node_id;
    }

    out
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
