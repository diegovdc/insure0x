["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/structs/queue.js"],"~:js","goog.provide(\"goog.structs.Queue\");\ngoog.require(\"goog.array\");\ngoog.structs.Queue = function() {\n  this.front_ = [];\n  this.back_ = [];\n};\ngoog.structs.Queue.prototype.maybeFlip_ = function() {\n  if (goog.array.isEmpty(this.front_)) {\n    this.front_ = this.back_;\n    this.front_.reverse();\n    this.back_ = [];\n  }\n};\ngoog.structs.Queue.prototype.enqueue = function(element) {\n  this.back_.push(element);\n};\ngoog.structs.Queue.prototype.dequeue = function() {\n  this.maybeFlip_();\n  return this.front_.pop();\n};\ngoog.structs.Queue.prototype.peek = function() {\n  this.maybeFlip_();\n  return goog.array.peek(this.front_);\n};\ngoog.structs.Queue.prototype.getCount = function() {\n  return this.front_.length + this.back_.length;\n};\ngoog.structs.Queue.prototype.isEmpty = function() {\n  return goog.array.isEmpty(this.front_) && goog.array.isEmpty(this.back_);\n};\ngoog.structs.Queue.prototype.clear = function() {\n  this.front_ = [];\n  this.back_ = [];\n};\ngoog.structs.Queue.prototype.contains = function(obj) {\n  return goog.array.contains(this.front_, obj) || goog.array.contains(this.back_, obj);\n};\ngoog.structs.Queue.prototype.remove = function(obj) {\n  return goog.array.removeLast(this.front_, obj) || goog.array.remove(this.back_, obj);\n};\ngoog.structs.Queue.prototype.getValues = function() {\n  var res = [];\n  for (var i = this.front_.length - 1; i >= 0; --i) {\n    res.push(this.front_[i]);\n  }\n  var len = this.back_.length;\n  for (var i = 0; i < len; ++i) {\n    res.push(this.back_[i]);\n  }\n  return res;\n};\n","~:source","// Copyright 2006 The Closure Library Authors. All Rights Reserved.\n//\n// Licensed under the Apache License, Version 2.0 (the \"License\");\n// you may not use this file except in compliance with the License.\n// You may obtain a copy of the License at\n//\n//      http://www.apache.org/licenses/LICENSE-2.0\n//\n// Unless required by applicable law or agreed to in writing, software\n// distributed under the License is distributed on an \"AS-IS\" BASIS,\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n// See the License for the specific language governing permissions and\n// limitations under the License.\n\n/**\n * @fileoverview Datastructure: Queue.\n *\n *\n * This file provides the implementation of a FIFO Queue structure.\n * API is similar to that of com.google.common.collect.IntQueue\n *\n * The implementation is a classic 2-stack queue.\n * There's a \"front\" stack and a \"back\" stack.\n * Items are pushed onto \"back\" and popped from \"front\".\n * When \"front\" is empty, we replace \"front\" with reverse(back).\n *\n * Example:\n * front                         back            op\n * []                            []              enqueue 1\n * []                            [1]             enqueue 2\n * []                            [1,2]           enqueue 3\n * []                            [1,2,3]         dequeue -> ...\n * [3,2,1]                       []              ... -> 1\n * [3,2]                         []              enqueue 4\n * [3,2]                         [4]             dequeue -> 2\n * [3]                           [4]\n *\n * Front and back are simple javascript arrays. We rely on\n * Array.push and Array.pop being O(1) amortized.\n *\n * Note: In V8, queues, up to a certain size, can be implemented\n * just fine using Array.push and Array.shift, but other JavaScript\n * engines do not have the optimization of Array.shift.\n *\n */\n\ngoog.provide('goog.structs.Queue');\n\ngoog.require('goog.array');\n\n\n\n/**\n * Class for FIFO Queue data structure.\n *\n * @constructor\n * @template T\n */\ngoog.structs.Queue = function() {\n  /**\n   * @private {!Array<T>} Front stack. Items are pop()'ed from here.\n   */\n  this.front_ = [];\n  /**\n   * @private {!Array<T>} Back stack. Items are push()'ed here.\n   */\n  this.back_ = [];\n};\n\n\n/**\n * Flips the back stack onto the front stack if front is empty,\n * to prepare for peek() or dequeue().\n *\n * @private\n */\ngoog.structs.Queue.prototype.maybeFlip_ = function() {\n  if (goog.array.isEmpty(this.front_)) {\n    this.front_ = this.back_;\n    this.front_.reverse();\n    this.back_ = [];\n  }\n};\n\n\n/**\n * Puts the specified element on this queue.\n * @param {T} element The element to be added to the queue.\n */\ngoog.structs.Queue.prototype.enqueue = function(element) {\n  this.back_.push(element);\n};\n\n\n/**\n * Retrieves and removes the head of this queue.\n * @return {T} The element at the head of this queue. Returns undefined if the\n *     queue is empty.\n */\ngoog.structs.Queue.prototype.dequeue = function() {\n  this.maybeFlip_();\n  return this.front_.pop();\n};\n\n\n/**\n * Retrieves but does not remove the head of this queue.\n * @return {T} The element at the head of this queue. Returns undefined if the\n *     queue is empty.\n */\ngoog.structs.Queue.prototype.peek = function() {\n  this.maybeFlip_();\n  return goog.array.peek(this.front_);\n};\n\n\n/**\n * Returns the number of elements in this queue.\n * @return {number} The number of elements in this queue.\n */\ngoog.structs.Queue.prototype.getCount = function() {\n  return this.front_.length + this.back_.length;\n};\n\n\n/**\n * Returns true if this queue contains no elements.\n * @return {boolean} true if this queue contains no elements.\n */\ngoog.structs.Queue.prototype.isEmpty = function() {\n  return goog.array.isEmpty(this.front_) && goog.array.isEmpty(this.back_);\n};\n\n\n/**\n * Removes all elements from the queue.\n */\ngoog.structs.Queue.prototype.clear = function() {\n  this.front_ = [];\n  this.back_ = [];\n};\n\n\n/**\n * Returns true if the given value is in the queue.\n * @param {T} obj The value to look for.\n * @return {boolean} Whether the object is in the queue.\n */\ngoog.structs.Queue.prototype.contains = function(obj) {\n  return goog.array.contains(this.front_, obj) ||\n      goog.array.contains(this.back_, obj);\n};\n\n\n/**\n * Removes the first occurrence of a particular value from the queue.\n * @param {T} obj Object to remove.\n * @return {boolean} True if an element was removed.\n */\ngoog.structs.Queue.prototype.remove = function(obj) {\n  return goog.array.removeLast(this.front_, obj) ||\n      goog.array.remove(this.back_, obj);\n};\n\n\n/**\n * Returns all the values in the queue.\n * @return {!Array<T>} An array of the values in the queue.\n */\ngoog.structs.Queue.prototype.getValues = function() {\n  var res = [];\n  // Add the front array in reverse, then the back array.\n  for (var i = this.front_.length - 1; i >= 0; --i) {\n    res.push(this.front_[i]);\n  }\n  var len = this.back_.length;\n  for (var i = 0; i < len; ++i) {\n    res.push(this.back_[i]);\n  }\n  return res;\n};\n","~:compiled-at",1600144468905,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.structs.queue.js\",\n\"lineCount\":52,\n\"mappings\":\"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\",\n\"sources\":[\"goog/structs/queue.js\"],\n\"sourcesContent\":[\"// Copyright 2006 The Closure Library Authors. All Rights Reserved.\\n//\\n// Licensed under the Apache License, Version 2.0 (the \\\"License\\\");\\n// you may not use this file except in compliance with the License.\\n// You may obtain a copy of the License at\\n//\\n//      http://www.apache.org/licenses/LICENSE-2.0\\n//\\n// Unless required by applicable law or agreed to in writing, software\\n// distributed under the License is distributed on an \\\"AS-IS\\\" BASIS,\\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\\n// See the License for the specific language governing permissions and\\n// limitations under the License.\\n\\n/**\\n * @fileoverview Datastructure: Queue.\\n *\\n *\\n * This file provides the implementation of a FIFO Queue structure.\\n * API is similar to that of com.google.common.collect.IntQueue\\n *\\n * The implementation is a classic 2-stack queue.\\n * There's a \\\"front\\\" stack and a \\\"back\\\" stack.\\n * Items are pushed onto \\\"back\\\" and popped from \\\"front\\\".\\n * When \\\"front\\\" is empty, we replace \\\"front\\\" with reverse(back).\\n *\\n * Example:\\n * front                         back            op\\n * []                            []              enqueue 1\\n * []                            [1]             enqueue 2\\n * []                            [1,2]           enqueue 3\\n * []                            [1,2,3]         dequeue -> ...\\n * [3,2,1]                       []              ... -> 1\\n * [3,2]                         []              enqueue 4\\n * [3,2]                         [4]             dequeue -> 2\\n * [3]                           [4]\\n *\\n * Front and back are simple javascript arrays. We rely on\\n * Array.push and Array.pop being O(1) amortized.\\n *\\n * Note: In V8, queues, up to a certain size, can be implemented\\n * just fine using Array.push and Array.shift, but other JavaScript\\n * engines do not have the optimization of Array.shift.\\n *\\n */\\n\\ngoog.provide('goog.structs.Queue');\\n\\ngoog.require('goog.array');\\n\\n\\n\\n/**\\n * Class for FIFO Queue data structure.\\n *\\n * @constructor\\n * @template T\\n */\\ngoog.structs.Queue = function() {\\n  /**\\n   * @private {!Array<T>} Front stack. Items are pop()'ed from here.\\n   */\\n  this.front_ = [];\\n  /**\\n   * @private {!Array<T>} Back stack. Items are push()'ed here.\\n   */\\n  this.back_ = [];\\n};\\n\\n\\n/**\\n * Flips the back stack onto the front stack if front is empty,\\n * to prepare for peek() or dequeue().\\n *\\n * @private\\n */\\ngoog.structs.Queue.prototype.maybeFlip_ = function() {\\n  if (goog.array.isEmpty(this.front_)) {\\n    this.front_ = this.back_;\\n    this.front_.reverse();\\n    this.back_ = [];\\n  }\\n};\\n\\n\\n/**\\n * Puts the specified element on this queue.\\n * @param {T} element The element to be added to the queue.\\n */\\ngoog.structs.Queue.prototype.enqueue = function(element) {\\n  this.back_.push(element);\\n};\\n\\n\\n/**\\n * Retrieves and removes the head of this queue.\\n * @return {T} The element at the head of this queue. Returns undefined if the\\n *     queue is empty.\\n */\\ngoog.structs.Queue.prototype.dequeue = function() {\\n  this.maybeFlip_();\\n  return this.front_.pop();\\n};\\n\\n\\n/**\\n * Retrieves but does not remove the head of this queue.\\n * @return {T} The element at the head of this queue. Returns undefined if the\\n *     queue is empty.\\n */\\ngoog.structs.Queue.prototype.peek = function() {\\n  this.maybeFlip_();\\n  return goog.array.peek(this.front_);\\n};\\n\\n\\n/**\\n * Returns the number of elements in this queue.\\n * @return {number} The number of elements in this queue.\\n */\\ngoog.structs.Queue.prototype.getCount = function() {\\n  return this.front_.length + this.back_.length;\\n};\\n\\n\\n/**\\n * Returns true if this queue contains no elements.\\n * @return {boolean} true if this queue contains no elements.\\n */\\ngoog.structs.Queue.prototype.isEmpty = function() {\\n  return goog.array.isEmpty(this.front_) && goog.array.isEmpty(this.back_);\\n};\\n\\n\\n/**\\n * Removes all elements from the queue.\\n */\\ngoog.structs.Queue.prototype.clear = function() {\\n  this.front_ = [];\\n  this.back_ = [];\\n};\\n\\n\\n/**\\n * Returns true if the given value is in the queue.\\n * @param {T} obj The value to look for.\\n * @return {boolean} Whether the object is in the queue.\\n */\\ngoog.structs.Queue.prototype.contains = function(obj) {\\n  return goog.array.contains(this.front_, obj) ||\\n      goog.array.contains(this.back_, obj);\\n};\\n\\n\\n/**\\n * Removes the first occurrence of a particular value from the queue.\\n * @param {T} obj Object to remove.\\n * @return {boolean} True if an element was removed.\\n */\\ngoog.structs.Queue.prototype.remove = function(obj) {\\n  return goog.array.removeLast(this.front_, obj) ||\\n      goog.array.remove(this.back_, obj);\\n};\\n\\n\\n/**\\n * Returns all the values in the queue.\\n * @return {!Array<T>} An array of the values in the queue.\\n */\\ngoog.structs.Queue.prototype.getValues = function() {\\n  var res = [];\\n  // Add the front array in reverse, then the back array.\\n  for (var i = this.front_.length - 1; i >= 0; --i) {\\n    res.push(this.front_[i]);\\n  }\\n  var len = this.back_.length;\\n  for (var i = 0; i < len; ++i) {\\n    res.push(this.back_[i]);\\n  }\\n  return res;\\n};\\n\"],\n\"names\":[\"goog\",\"provide\",\"require\",\"structs\",\"Queue\",\"goog.structs.Queue\",\"front_\",\"back_\",\"prototype\",\"maybeFlip_\",\"goog.structs.Queue.prototype.maybeFlip_\",\"array\",\"isEmpty\",\"reverse\",\"enqueue\",\"goog.structs.Queue.prototype.enqueue\",\"element\",\"push\",\"dequeue\",\"goog.structs.Queue.prototype.dequeue\",\"pop\",\"peek\",\"goog.structs.Queue.prototype.peek\",\"getCount\",\"goog.structs.Queue.prototype.getCount\",\"length\",\"goog.structs.Queue.prototype.isEmpty\",\"clear\",\"goog.structs.Queue.prototype.clear\",\"contains\",\"goog.structs.Queue.prototype.contains\",\"obj\",\"remove\",\"goog.structs.Queue.prototype.remove\",\"removeLast\",\"getValues\",\"goog.structs.Queue.prototype.getValues\",\"res\",\"i\",\"len\"]\n}\n"]