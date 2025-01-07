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

#[cfg(test)]
mod tests {
    use oxc_allocator::Allocator;
    use oxc_ast::ast::SourceType;
    use oxc_codegen::{CodeGenerator, CodegenOptions};
    use oxc_parser::Parser;
    use oxsea_from_ast::ir_from_ast;
    use oxsea_into_ast::ir_into_ast;
    use oxsea_ir::IRGraph;

    fn ir_from_source(source: &str) -> oxsea_ir::IRGraph {
        let allocator = Allocator::default();
        let ret = Parser::new(&allocator, source, SourceType::tsx()).parse();
        ir_from_ast(&ret.program).unwrap()
    }

    fn convert_and_print(graph: &IRGraph) -> String {
        let allocator = Allocator::default();
        let program = ir_into_ast(&allocator, graph, true).unwrap();
        let mut options = CodegenOptions::default();
        options.single_quote = true;
        options.minify = true;
        let printed = CodeGenerator::new()
            .with_options(options.clone())
            .build(&program);
        printed.code
    }

    #[test]
    fn roundtrips_a_function() {
        let source = "function f(){return 2}";
        let ir = ir_from_source(source);
        let actual = convert_and_print(&ir);
        assert_eq!(actual, source);
    }
}
