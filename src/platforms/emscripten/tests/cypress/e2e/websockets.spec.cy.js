/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 by Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */
describe("websockets", () => {
  beforeEach(() => {
    // Use Docker echo server in CI, otherwise use the default online server
    const ciEnv = Cypress.env('CI');
    const isCI = ciEnv === true || ciEnv === 'true';
    const url = isCI
      ? "/tests/src/test_websockets.html#echo_server=ws://localhost:9090"
      : "/tests/src/test_websockets.html";

    cy.visit(url);
  });

  it("should pass test", () => {
    cy.get("#result").should('contain', 'Test success');
  });
});
