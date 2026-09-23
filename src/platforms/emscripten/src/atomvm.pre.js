/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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
Module["cast"] = function (name, message) {
  ccall("cast", "void", ["string", "string"], [name, message]);
};
Module["call"] = async function (name, message) {
  const promiseId = ccall(
    "call",
    "integer",
    ["string", "string"],
    [name, message],
  );
  return promiseMap.get(promiseId).promise;
};
Module["nextTrackedObjectKey"] = function () {
  // the raw i32 from ccall is signed; keys are unsigned
  return ccall("next_tracked_object_key", "integer", [], []) >>> 0;
};
Module["trackedObjectsMap"] = new Map();
Module["onTrackedObjectDelete"] = (key) => {
  Module["trackedObjectsMap"].delete(key);
};
Module["onGetTrackedObjects"] = (keys) => {
  const getTrackedObject = (key) => Module["trackedObjectsMap"].get(key);
  return keys.map(getTrackedObject);
};
// mirrors TRACKED_OBJECT_KEY_EXHAUSTED in emscripten_sys.h
const trackedObjectKeyExhausted = 4294967295;
Module["onRunTrackedJs"] = (scriptString, isDebug) => {
  const trackedKeys = [];
  const trackValue = (value) => {
    const key = Module["nextTrackedObjectKey"]();
    if (key === trackedObjectKeyExhausted) {
      throw new Error("tracked object key space is exhausted");
    }
    Module["trackedObjectsMap"].set(key, value);
    trackedKeys[trackedKeys.length] = key;
    return key;
  };

  let result;
  try {
    const indirectEval = eval;
    result = indirectEval(scriptString);
  } catch (e) {
    isDebug && console.error("onRunTrackedJs: evaluated script threw", e);
    return null;
  }
  if (result === null || result === undefined) {
    return [];
  }
  if (!Array.isArray(result)) {
    isDebug &&
      console.error(
        "onRunTrackedJs: script must evaluate to an array, null or undefined; got",
        result,
      );
    return null;
  }
  try {
    // Array.from maps holes in sparse arrays; result.map would leave them,
    // and a hole reads back as key 0
    return Array.from(result, trackValue);
  } catch (e) {
    // Reading the array can throw halfway through (an exotic iterator, an
    // element getter, or trackValue on an exhausted key space), leaving
    // values tracked under keys the caller never gets: give them up.
    isDebug && console.error("onRunTrackedJs: tracking the result threw", e);
    for (let i = 0; i < trackedKeys.length; ++i) {
      try {
        Module["onTrackedObjectDelete"](trackedKeys[i]);
      } catch (ignored) {
        // keep dropping the remaining keys
      }
    }
    return null;
  }
};
