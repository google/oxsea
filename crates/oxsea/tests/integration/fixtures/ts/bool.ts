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

declare global {
  var ga: number;
  var gb: number;
}

let two = 2;
let three = 3;

export let t = true;
export let f = false;
export let eqT = two === two;
export let eqF = two === three;
export let eqU = two === gb;
export let eqUR = ga === three;
export let leqT = two == two;
export let leqF = two == three;
export let leqU = two == gb;
export let leqUR = ga == three;
