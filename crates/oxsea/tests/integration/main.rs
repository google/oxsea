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

use oxc_allocator::Allocator;
use oxc_ast::ast::SourceType;
use oxc_codegen::{CodeGenerator, CodegenOptions};
use oxc_parser::Parser;
use oxsea_from_ast::ir_from_ast;
use oxsea_into_ast::ir_into_ast;
use oxsea_ir::{ir_to_dot, IRGraph};
use oxsea_traverse::transform::{transform, Transform};
use std::fs;

fn ir_from_source(source: &str) -> oxsea_ir::IRGraph {
    let allocator = Allocator::default();
    let ret = Parser::new(&allocator, source, SourceType::tsx()).parse();
    ir_from_ast(&ret.program).unwrap()
}

fn convert_and_print(graph: &IRGraph) -> String {
    let allocator = Allocator::default();
    let program = ir_into_ast(&allocator, graph, false).unwrap();
    let mut options = CodegenOptions::default();
    options.single_quote = true;
    let printed = CodeGenerator::new()
        .with_options(options.clone())
        .build(&program);
    printed.code
}

struct CopyTransform {}

impl Transform for CopyTransform {}

fn copy_ir(ir: &IRGraph) -> IRGraph {
    let mut copy_transform = CopyTransform {};
    transform(&ir, &mut copy_transform)
}

#[test]
fn main() {
    insta::glob!("fixtures/**/*.{js,ts,jsx,tsx}", |path| {
        let code = fs::read_to_string(path).unwrap();
        let name = path.file_stem().unwrap().to_str().unwrap();
        let ir = ir_from_source(&code);

        let dot_snapshot = format!("{}\n", ir_to_dot(&ir));
        insta::with_settings!({ snapshot_path => path.parent().unwrap(), prepend_module_to_snapshot => false, snapshot_suffix => "ir" }, {
            insta::assert_snapshot!(name, dot_snapshot);
        });

        let code_snapshot = convert_and_print(&ir);
        insta::with_settings!({ snapshot_path => path.parent().unwrap(), prepend_module_to_snapshot => false, snapshot_suffix => "out" }, {
            insta::assert_snapshot!(name, code_snapshot);
        });

        let ir_copy = copy_ir(&ir);
        let copy_dot_snapshot = format!("{}\n", ir_to_dot(&ir_copy));
        insta::with_settings!({ snapshot_path => path.parent().unwrap(), prepend_module_to_snapshot => false, snapshot_suffix => "copy" }, {
            insta::assert_snapshot!(name, copy_dot_snapshot);
        });
    });
}
