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
describe('emscripten examples homepage', () => {
    it('should list examples', () => {
        cy.visit('/')
        cy.contains('Hello world')
        cy.contains('Run script')
        cy.contains('Call & cast')
    })
});

describe('emscripten hello world AVM', () => {
    it('should output to console', () => {
        cy.visit('/hello_world_avm.html', {
            onBeforeLoad(win) {
                cy.stub(win.console, 'log').as('consoleLog')
                cy.stub(win.console, 'error').as('consoleError')
            }
        })
        cy.get('@consoleLog').should('be.calledWith', 'Hello World')
        cy.get('@consoleLog').should('be.calledWith', 'Return value: ok')
        cy.get('@consoleError').should('not.be.called')
    })
});

describe('emscripten hello world BEAM', () => {
    it('should output to console', () => {
        cy.visit('/hello_world_beam.html', {
            onBeforeLoad(win) {
                cy.stub(win.console, 'log').as('consoleLog')
                cy.stub(win.console, 'error').as('consoleError')
            }
        })
        cy.get('@consoleLog').should('be.calledWith', 'Hello World')
        cy.get('@consoleLog').should('be.calledWith', 'Return value: ok')
        cy.get('@consoleError').should('not.be.called')
    })
});

describe('emscripten run script', () => {
    it('should display three alerts and update counter', () => {
        const alert = cy.stub().as("alert")
        cy.on('window:alert', alert)
        cy.visit('/run_script.html')
        cy.get('#demo-counter').should('contain', 'unset')
        cy.get("@alert").should("have.been.calledWith", "hello from Erlang in main thread")
        cy.get("@alert").should("have.been.calledWithMatch", "hello from Erlang in worker thread")
        cy.get("@alert").should("have.been.calledWith", "hello from Erlang in main thread async")
        cy.get('#demo-counter').should('contain', 'ms')
    })
});

describe('emscripten call & cast', () => {
    it('should update counters when clicked', () => {
        cy.visit('/call_cast.html')
        cy.get('#call-counter').should('contain', '0')
        cy.get('button#call-button').click()
        cy.get('#call-counter').should('contain', '1')
        cy.get('button#call-button').click()
        cy.get('#call-counter').should('contain', '2')
        cy.get('button#call-button').click()
        cy.get('#call-counter').should('contain', '3')
        cy.get('button#call-button').click()
        cy.get('#call-counter').should('contain', '4')
        cy.get('button#call-button').click()
        cy.get('#call-counter').should('contain', '5')
        cy.get('#cast-counter').should('contain', '0')
        cy.get('button#cast-button').click()
        cy.get('#cast-counter').should('contain', '1')
        cy.get('#call-counter').should('contain', '5')
    })
});
