["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/structs/node.js"],"~:js","goog.provide(\"goog.structs.Node\");\ngoog.structs.Node = function(key, value) {\n  this.key_ = key;\n  this.value_ = value;\n};\ngoog.structs.Node.prototype.getKey = function() {\n  return this.key_;\n};\ngoog.structs.Node.prototype.getValue = function() {\n  return this.value_;\n};\ngoog.structs.Node.prototype.clone = function() {\n  return new goog.structs.Node(this.key_, this.value_);\n};\n","~:source","// Copyright 2006 The Closure Library Authors. All Rights Reserved.\n//\n// Licensed under the Apache License, Version 2.0 (the \"License\");\n// you may not use this file except in compliance with the License.\n// You may obtain a copy of the License at\n//\n//      http://www.apache.org/licenses/LICENSE-2.0\n//\n// Unless required by applicable law or agreed to in writing, software\n// distributed under the License is distributed on an \"AS-IS\" BASIS,\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n// See the License for the specific language governing permissions and\n// limitations under the License.\n\n/**\n * @fileoverview Generic immutable node object to be used in collections.\n *\n */\n\n\ngoog.provide('goog.structs.Node');\n\n\n\n/**\n * A generic immutable node. This can be used in various collections that\n * require a node object for its item (such as a heap).\n * @param {K} key Key.\n * @param {V} value Value.\n * @constructor\n * @template K, V\n */\ngoog.structs.Node = function(key, value) {\n  /**\n   * The key.\n   * @private {K}\n   */\n  this.key_ = key;\n\n  /**\n   * The value.\n   * @private {V}\n   */\n  this.value_ = value;\n};\n\n\n/**\n * Gets the key.\n * @return {K} The key.\n */\ngoog.structs.Node.prototype.getKey = function() {\n  return this.key_;\n};\n\n\n/**\n * Gets the value.\n * @return {V} The value.\n */\ngoog.structs.Node.prototype.getValue = function() {\n  return this.value_;\n};\n\n\n/**\n * Clones a node and returns a new node.\n * @return {!goog.structs.Node<K, V>} A new goog.structs.Node with the same\n *     key value pair.\n */\ngoog.structs.Node.prototype.clone = function() {\n  return new goog.structs.Node(this.key_, this.value_);\n};\n","~:compiled-at",1600144468909,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.structs.node.js\",\n\"lineCount\":15,\n\"mappings\":\"AAoBAA,IAAA,CAAKC,OAAL,CAAa,mBAAb,CAAA;AAYAD,IAAA,CAAKE,OAAL,CAAaC,IAAb,GAAoBC,QAAQ,CAACC,GAAD,EAAMC,KAAN,CAAa;AAKvC,MAAA,CAAKC,IAAL,GAAYF,GAAZ;AAMA,MAAA,CAAKG,MAAL,GAAcF,KAAd;AAXuC,CAAzC;AAmBAN,IAAA,CAAKE,OAAL,CAAaC,IAAb,CAAkBM,SAAlB,CAA4BC,MAA5B,GAAqCC,QAAQ,EAAG;AAC9C,SAAO,IAAP,CAAYJ,IAAZ;AAD8C,CAAhD;AASAP,IAAA,CAAKE,OAAL,CAAaC,IAAb,CAAkBM,SAAlB,CAA4BG,QAA5B,GAAuCC,QAAQ,EAAG;AAChD,SAAO,IAAP,CAAYL,MAAZ;AADgD,CAAlD;AAUAR,IAAA,CAAKE,OAAL,CAAaC,IAAb,CAAkBM,SAAlB,CAA4BK,KAA5B,GAAoCC,QAAQ,EAAG;AAC7C,SAAO,IAAIf,IAAJ,CAASE,OAAT,CAAiBC,IAAjB,CAAsB,IAAtB,CAA2BI,IAA3B,EAAiC,IAAjC,CAAsCC,MAAtC,CAAP;AAD6C,CAA/C;;\",\n\"sources\":[\"goog/structs/node.js\"],\n\"sourcesContent\":[\"// Copyright 2006 The Closure Library Authors. All Rights Reserved.\\n//\\n// Licensed under the Apache License, Version 2.0 (the \\\"License\\\");\\n// you may not use this file except in compliance with the License.\\n// You may obtain a copy of the License at\\n//\\n//      http://www.apache.org/licenses/LICENSE-2.0\\n//\\n// Unless required by applicable law or agreed to in writing, software\\n// distributed under the License is distributed on an \\\"AS-IS\\\" BASIS,\\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\\n// See the License for the specific language governing permissions and\\n// limitations under the License.\\n\\n/**\\n * @fileoverview Generic immutable node object to be used in collections.\\n *\\n */\\n\\n\\ngoog.provide('goog.structs.Node');\\n\\n\\n\\n/**\\n * A generic immutable node. This can be used in various collections that\\n * require a node object for its item (such as a heap).\\n * @param {K} key Key.\\n * @param {V} value Value.\\n * @constructor\\n * @template K, V\\n */\\ngoog.structs.Node = function(key, value) {\\n  /**\\n   * The key.\\n   * @private {K}\\n   */\\n  this.key_ = key;\\n\\n  /**\\n   * The value.\\n   * @private {V}\\n   */\\n  this.value_ = value;\\n};\\n\\n\\n/**\\n * Gets the key.\\n * @return {K} The key.\\n */\\ngoog.structs.Node.prototype.getKey = function() {\\n  return this.key_;\\n};\\n\\n\\n/**\\n * Gets the value.\\n * @return {V} The value.\\n */\\ngoog.structs.Node.prototype.getValue = function() {\\n  return this.value_;\\n};\\n\\n\\n/**\\n * Clones a node and returns a new node.\\n * @return {!goog.structs.Node<K, V>} A new goog.structs.Node with the same\\n *     key value pair.\\n */\\ngoog.structs.Node.prototype.clone = function() {\\n  return new goog.structs.Node(this.key_, this.value_);\\n};\\n\"],\n\"names\":[\"goog\",\"provide\",\"structs\",\"Node\",\"goog.structs.Node\",\"key\",\"value\",\"key_\",\"value_\",\"prototype\",\"getKey\",\"goog.structs.Node.prototype.getKey\",\"getValue\",\"goog.structs.Node.prototype.getValue\",\"clone\",\"goog.structs.Node.prototype.clone\"]\n}\n"]