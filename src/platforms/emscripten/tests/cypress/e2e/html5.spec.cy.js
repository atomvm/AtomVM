/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Paul Guyot <pguyot@kallisys.net>
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
describe("html5", () => {
  beforeEach(() => {
    cy.visit("/tests/src/test_html5.html");
  });

  it("should process several events", () => {
    cy.get("#register-and-loop").click()
    cy.get("#result").find('span.timestamp').should('have.length', 1)
    cy.get("#register-once-and-exit").click()
    cy.get("#result").find('span.timestamp').should('have.length', 2)
  });
  it("should not crash or leak when process dies", () => {
    cy.get("#memory-binary").click()
    cy.get("#register-once-and-exit").click()
    cy.get("#result").find('span.timestamp').should('have.length', 1)
    cy.get("#register-once-and-exit").click()
    cy.get("#result").find('span.timestamp').should('have.length', 1)
    cy.get("#memory-binary").click()
    cy.get("#result span.binary:first(1)").invoke('text').then((firstBinaryText) => {
        cy.get("#result span.binary:last(2)").invoke('text').then((lastBinaryText) => {
            expect(parseInt(lastBinaryText.trim())).below(parseInt(firstBinaryText.trim()))
        });
    });
  });
});
