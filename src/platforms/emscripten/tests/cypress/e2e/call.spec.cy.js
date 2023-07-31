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
describe("call", () => {
  beforeEach(() => {
    cy.visit("/tests/src/test_call.html");
  });

  it("should resolve an integer", () => {
    cy.get("#target-input").type("main");
    cy.get("#message-input").type("resolve-42");
    cy.get("#call-button").click();
    cy.get("#result").should("contain", "42");
  });

  it("should resolve a string", () => {
    cy.get("#target-input").type("main");
    cy.get("#message-input").type("resolve-ok");
    cy.get("#call-button").click();
    cy.get("#result").should("contain", "ok");
  });

  it("should reject an integer", () => {
    cy.get("#target-input").type("main");
    cy.get("#message-input").type("reject-42");
    cy.get("#call-button").click();
    cy.get("#error").should("contain", "42");
  });

  it("should reject a string", () => {
    cy.get("#target-input").type("main");
    cy.get("#message-input").type("reject-ok");
    cy.get("#call-button").click();
    cy.get("#error").should("contain", "ok");
  });

  it("should reject an unknown process", () => {
    cy.get("#target-input").type("unknown");
    cy.get("#message-input").type("resolve-ok");
    cy.get("#call-button").click();
    cy.get("#error").should("contain", "noproc");
  });

  it("should reject a garbage collected promise", () => {
    cy.get("#target-input").type("main");
    cy.get("#message-input").type("ignore");
    cy.get("#call-button").click();
    cy.get("#error").should("contain", "noproc");
  });
});
