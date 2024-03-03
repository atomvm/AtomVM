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
describe("emscripten examples homepage", () => {
  it("should list examples", () => {
    cy.visit("/");
    cy.contains("Hello world");
    cy.contains("Run script");
    cy.contains("Call & cast");
    cy.contains("HTML5 Events");
  });
});

describe("emscripten hello world AVM", () => {
  it("should output to console", () => {
    cy.visit("/hello_world_avm.html", {
      onBeforeLoad(win) {
        cy.stub(win.console, "log").as("consoleLog");
        cy.stub(win.console, "error").as("consoleError");
      },
    });
    cy.get("@consoleLog").should("be.calledWith", "hello_world");
    cy.get("@consoleLog").should("be.calledWith", "Return value: ok");
    cy.get("@consoleError").should("not.be.called");
  });
});

describe("emscripten hello world BEAM", () => {
  it("should output to console", () => {
    cy.visit("/hello_world_beam.html", {
      onBeforeLoad(win) {
        cy.stub(win.console, "log").as("consoleLog");
        cy.stub(win.console, "error").as("consoleError");
      },
    });
    cy.get("@consoleLog").should("be.calledWith", "hello_world");
    cy.get("@consoleLog").should("be.calledWith", "Return value: ok");
    cy.get("@consoleError").should("not.be.called");
  });
});

describe("emscripten run script", () => {
  it("should display three alerts and update counter", () => {
    const alert = cy.stub().as("alert");
    cy.on("window:alert", alert);
    cy.visit("/run_script.html");
    cy.get("#demo-counter").should("contain", "unset");
    cy.get("@alert").should(
      "have.been.calledWith",
      "hello from Erlang in main thread",
    );
    cy.get("@alert").should(
      "have.been.calledWithMatch",
      "hello from Erlang in worker thread",
    );
    cy.get("@alert").should(
      "have.been.calledWith",
      "hello from Erlang in main thread async",
    );
    cy.get("#demo-counter").should("contain", "ms");
  });
});

describe("emscripten call & cast", () => {
  it("should update counters when clicked", () => {
    cy.visit("/call_cast.html");
    cy.get("#call-counter").should("contain", "0");
    cy.get("button#call-button").click();
    cy.get("#call-counter").should("contain", "1");
    cy.get("button#call-button").click();
    cy.get("#call-counter").should("contain", "2");
    cy.get("button#call-button").click();
    cy.get("#call-counter").should("contain", "3");
    cy.get("button#call-button").click();
    cy.get("#call-counter").should("contain", "4");
    cy.get("button#call-button").click();
    cy.get("#call-counter").should("contain", "5");
    cy.get("#cast-counter").should("contain", "0");
    cy.get("button#cast-button").click();
    cy.get("#cast-counter").should("contain", "1");
    cy.get("#call-counter").should("contain", "5");
  });
});

describe("emscripten HTML5 Events", () => {
  it("should work with key events", () => {
    cy.visit("/html5_events.html");
    cy.get("#keypress-checkbox").click();
    cy.get("body").type("A");
    cy.get("#event-1 td.event-type").should("contain", "keypress");
    cy.get("#event-1 td.event-map").should("contain", "KeyA");
    cy.get("#event-1 td.event-user-data").should("contain", "document");
    cy.get("#event-2 td.event-type").should("contain", "keypress");
    cy.get("#keypress-checkbox").click();

    cy.get("#keyup-checkbox").click();
    cy.get("body").type("B");
    cy.get("#event-3 td.event-type").should("contain", "keyup");
    cy.get("#event-3 td.event-map").should("contain", "KeyB");
    cy.get("#event-3 td.event-user-data").should("contain", "document");
    cy.get("#event-4 td.event-type").should("contain", "keyup");
    cy.get("#keyup-checkbox").click();

    cy.get("#keydown-checkbox").click();
    cy.get("body").type("C");
    cy.get("#event-5 td.event-type").should("contain", "keydown");
    cy.get("#event-5 td.event-map").should("contain", "KeyC");
    cy.get("#event-5 td.event-user-data").should("contain", "document");
    cy.get("#event-6 td.event-type").should("contain", "keydown");
    cy.get("#keydown-checkbox").click();

    cy.get("#keypress-checkbox").click();
    cy.get("#input-field").type("D");
    cy.get("#event-7 td.event-type").should("contain", "keypress");
    cy.get("#event-7 td.event-map").should("contain", "KeyD");
    cy.get("#event-7 td.event-user-data").should(
      "contain",
      "keypress -- input field",
    );
    cy.get("#event-8 td.event-type").should("contain", "keypress");
    cy.get("#event-8 td.event-user-data").should("contain", "document");
    cy.get("#event-9 td.event-type").should("contain", "keypress");
    cy.get("#keypress-checkbox").click();
    cy.get("#event-10").should("not.exist");
  });
  it("should work with click events with modifiers", () => {
    cy.visit("/html5_events.html");
    cy.get("#click-checkbox").click();
    cy.get("body").click({ altKey: true });
    cy.get("#event-1 td.event-type").should("contain", "click");
    cy.get("#event-1 td.event-map").should("contain", "alt_key => true");
    cy.get("#event-1 td.event-map").should("contain", "ctrl_key => false");
    cy.get("#event-1 td.event-map").should("contain", "meta_key => false");
    cy.get("#event-1 td.event-map").should("contain", "shift_key => false");
    cy.get("#event-1 td.event-user-data").should("contain", "document");
    cy.get("#event-2 td.event-type").should("contain", "click");
    cy.get("body").click({ ctrlKey: true });
    cy.get("#event-3 td.event-type").should("contain", "click");
    cy.get("#event-3 td.event-map").should("contain", "alt_key => false");
    cy.get("#event-3 td.event-map").should("contain", "ctrl_key => true");
    cy.get("#event-3 td.event-map").should("contain", "meta_key => false");
    cy.get("#event-3 td.event-map").should("contain", "shift_key => false");
    cy.get("#event-3 td.event-user-data").should("contain", "document");
    cy.get("#event-4 td.event-type").should("contain", "click");
    cy.get("body").click({ metaKey: true });
    cy.get("#event-5 td.event-type").should("contain", "click");
    cy.get("#event-5 td.event-map").should("contain", "alt_key => false");
    cy.get("#event-5 td.event-map").should("contain", "ctrl_key => false");
    cy.get("#event-5 td.event-map").should("contain", "meta_key => true");
    cy.get("#event-5 td.event-map").should("contain", "shift_key => false");
    cy.get("#event-5 td.event-user-data").should("contain", "document");
    cy.get("#event-6 td.event-type").should("contain", "click");
    cy.get("body").click({ shiftKey: true });
    cy.get("#event-7 td.event-type").should("contain", "click");
    cy.get("#event-7 td.event-map").should("contain", "alt_key => false");
    cy.get("#event-7 td.event-map").should("contain", "ctrl_key => false");
    cy.get("#event-7 td.event-map").should("contain", "meta_key => false");
    cy.get("#event-7 td.event-map").should("contain", "shift_key => true");
    cy.get("#event-7 td.event-user-data").should("contain", "document");
    cy.get("#event-8 td.event-type").should("contain", "click");
    cy.get("#click-checkbox").click();
    cy.get("#event-9 td.event-type").should("contain", "click");
    cy.get("#event-9 td.event-user-data").should("contain", "document");
    cy.get("#event-10 td.event-type").should("contain", "click");
    cy.get("#event-11").should("not.exist");
  });
  it("should work with click events with coordinates", () => {
    cy.visit("/html5_events.html");
    cy.get("#click-checkbox").click();
    cy.get("#input-field").click({ x: 2, y: 2 });
    cy.get("#event-1 td.event-type").should("contain", "click");
    // Could be offset by 1
    // Maybe related: https://github.com/cypress-io/cypress/issues/20802
    cy.get("#event-1 td.event-map").should(($eventMap) => {
        const eventMapText = $eventMap.text();
        expect(eventMapText).to.match(/target_x => [23]/).and.match(/target_y => [23]/);
    });
    cy.get("#event-1 td.event-user-data").should(
      "contain",
      "click -- input field",
    );
    cy.get("#event-2 td.event-type").should("contain", "click");
    cy.get("#event-2 td.event-user-data").should("contain", "document");
    cy.get("#event-3 td.event-type").should("contain", "click");
    cy.get("#click-checkbox").click();
    cy.get("#event-4 td.event-type").should("contain", "click");
    cy.get("#event-4 td.event-user-data").should("contain", "document");
    cy.get("#event-5 td.event-type").should("contain", "click");
    cy.get("#event-6").should("not.exist");
  });
  it("should work with dblclick events", () => {
    cy.visit("/html5_events.html");
    cy.get("#dblclick-checkbox").click();
    cy.get("body").dblclick();
    cy.get("#event-1 td.event-type").should("contain", "dblclick");
    cy.get("#event-1 td.event-user-data").should("contain", "document");
    cy.get("#event-2 td.event-type").should("contain", "click");
    cy.get("#dblclick-checkbox").click();
    cy.get("#event-3").should("not.exist");
  });
  it("should work with mousedown and mouseup events", () => {
    cy.visit("/html5_events.html");
    cy.get("#mousedown-checkbox").click();
    cy.get("#mouseup-checkbox").click();
    cy.get("#event-1 td.event-type").should("contain", "mousedown");
    cy.get("#event-1 td.event-user-data").should("contain", "document");
    cy.get("#event-2 td.event-type").should("contain", "mousedown");
    cy.get("body").click();
    cy.get("#event-3 td.event-type").should("contain", "mousedown");
    cy.get("#event-3 td.event-user-data").should("contain", "document");
    cy.get("#event-4 td.event-type").should("contain", "mousedown");
    cy.get("#event-5 td.event-type").should("contain", "mouseup");
    cy.get("#event-5 td.event-user-data").should("contain", "document");
    cy.get("#event-6 td.event-type").should("contain", "mouseup");
    cy.get("#event-7").should("not.exist");
  });
  it("should work with mousemove, mouseenter, mouseleave, mouseover and mouseout events", () => {
    cy.visit("/html5_events.html");
    cy.wrap([
      "mousemove",
      "mouseenter",
      "mouseleave",
      "mouseover",
      "mouseout",
    ]).each((event) => {
      cy.get(`#${event}-checkbox`).click();
      cy.get("h1").trigger(event);
      cy.get("#events tr td.event-type").should("contain", event);
    });
  });
  it("should work with wheel events", () => {
    cy.visit("/html5_events.html");
    cy.get(`#wheel-checkbox`).click();
    cy.get("#input-field").trigger("wheel");
    cy.get("#event-1 td.event-type").should("contain", "wheel");
    cy.get("#event-1 td.event-user-data").should(
      "contain",
      "wheel -- input field",
    );
    cy.get("#event-2 td.event-type").should("contain", "wheel");
    cy.get("#event-2 td.event-user-data").should("contain", "document");
    cy.get("#event-3 td.event-type").should("contain", "wheel");
    cy.get("#event-4").should("not.exist");
  });
  it("should work with resize events", () => {
    cy.visit("/html5_events.html");
    cy.get(`#resize-checkbox`).click();
    cy.get("#input-field").trigger("resize");
    cy.get("#event-1 td.event-type").should("contain", "resize");
    cy.get("#event-1 td.event-user-data").should(
      "contain",
      "resize -- input field",
    );
    cy.get("#event-2").should("not.exist");
  });
  it("should work with scroll events", () => {
    cy.visit("/html5_events.html");
    // Generate few events so we have something to scroll down to
    cy.wrap([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).each(() => {
        cy.get(`#click-checkbox`).click();
    });
    cy.get(`#scroll-checkbox`).click();
    cy.scrollTo(0, 500);
    cy.get("#event-11 td.event-type").should("contain", "scroll");
    cy.get("#event-11 td.event-user-data").should("contain", "document");
    cy.get("#event-12").should("not.exist");
  });
  it("should work with focus events", () => {
    cy.visit("/html5_events.html");
    cy.get(`#keypress-checkbox`).click();
    cy.wrap([
      { name: "blur", beforeKeypress: false },
      { name: "focus", beforeKeypress: true },
      { name: "focusin", beforeKeypress: true },
      { name: "focusout", beforeKeypress: false },
    ]).each((event) => {
      cy.get(`#${event.name}-checkbox`).click();
      cy.get("#input-field").focus().type("a").blur();
      cy.get(
        `#events tr:nth-child(${event.beforeKeypress ? 4 : 1}) td.event-type`,
      ).should("contain", event.name);
      cy.get(`#${event.name}-checkbox`).click();
    });
  });
  it("should work with touchstart, touchmove, touchend and touchcancel events", () => {
    cy.visit("/html5_events.html");
    cy.wrap(["touchstart", "touchend", "touchmove", "touchcancel"]).each(
      (event) => {
        cy.get(`#${event}-checkbox`).click();
        cy.get("body")
          .trigger("touchstart", 200, 50, {
            changedTouches: [
              {
                clientX: 200,
                clientY: 50,
              },
            ],
            targetTouches: [
              {
                clientX: 200,
                clientY: 50,
              },
            ],
            touches: [
              {
                clientX: 200,
                clientY: 50,
              },
            ],
          })
          .trigger("touchmove", 440, 50, {
            changedTouches: [
              {
                clientX: 440,
                clientY: 50,
              },
            ],
            targetTouches: [
              {
                clientX: 440,
                clientY: 50,
              },
            ],
            touches: [
              {
                clientX: 440,
                clientY: 50,
              },
            ],
          })
          .trigger(
            event === "touchcancel" ? "touchcancel" : "touchend",
            440,
            50,
            {
              changedTouches: [
                {
                  clientX: 440,
                  clientY: 50,
                },
              ],
              targetTouches: [
                {
                  clientX: 440,
                  clientY: 50,
                },
              ],
              touches: [
                {
                  clientX: 440,
                  clientY: 50,
                },
              ],
            },
          );
        cy.get("#events tr td.event-type").should("contain", event);
        cy.get(`#${event}-checkbox`).click();
      },
    );
  });
});
