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

// experimental API
Module["nextTrackedObjectKey"] = function () {
  return ccall("next_tracked_object_key", "integer", [], []);
};
Module["trackedObjectsMap"] = new Map();
Module["onTrackedObjectDelete"] = (key) => {
  Module["trackedObjectsMap"].delete(key);
};
Module["onGetTrackedObjects"] = (keys) => {
  const getTrackedObject = (key) => Module["trackedObjectsMap"].get(key);
  return keys.map(getTrackedObject);
};
Module["onRunTrackedJs"] = (scriptString, isDebug) => {
  const trackValue = (value) => {
    const key = Module["nextTrackedObjectKey"]();
    Module["trackedObjectsMap"].set(key, value);
    return key;
  };

  let result;
  try {
    const indirectEval = eval;
    result = indirectEval(scriptString);
  } catch (_e) {
    return null;
  }
  isDebug && ensureValidResult(result);
  return result?.map(trackValue) ?? [];
};

function ensureValidResult(result) {
  const isIndex = (k) => typeof k === "number";

  if (result === null) {
    return;
  }
  if (Array.isArray(result) && keys.every(isIndex)) {
    return;
  }

  const message =
    "Evaluated script returned invalid value. Expected number array or null";
  throw new Error(message);
}
