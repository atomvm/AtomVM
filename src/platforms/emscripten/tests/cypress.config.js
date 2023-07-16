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
const { defineConfig } = require('cypress')

module.exports = defineConfig({
  e2e: {
    supportFile: false,
    baseUrl: 'http://localhost:8080',
    video: false,
    setupNodeEvents(on, config) {
        // https://github.com/cypress-io/cypress/issues/19912
        on('before:browser:launch', (browser = {}, launchOptions) => {
            if (browser.family === 'chromium' && browser.name !== 'electron') {
              launchOptions.args.push('--enable-features=SharedArrayBuffer')
            }
            return launchOptions
          })
        },
    },
})
