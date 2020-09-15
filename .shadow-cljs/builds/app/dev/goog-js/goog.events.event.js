["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/events/event.js"],"~:js","goog.provide(\"goog.events.Event\");\ngoog.provide(\"goog.events.EventLike\");\ngoog.require(\"goog.Disposable\");\ngoog.require(\"goog.events.EventId\");\ngoog.events.EventLike;\ngoog.events.Event = function(type, opt_target) {\n  this.type = type instanceof goog.events.EventId ? String(type) : type;\n  this.target = opt_target;\n  this.currentTarget = this.target;\n  this.propagationStopped_ = false;\n  this.defaultPrevented = false;\n  this.returnValue_ = true;\n};\ngoog.events.Event.prototype.stopPropagation = function() {\n  this.propagationStopped_ = true;\n};\ngoog.events.Event.prototype.preventDefault = function() {\n  this.defaultPrevented = true;\n  this.returnValue_ = false;\n};\ngoog.events.Event.stopPropagation = function(e) {\n  e.stopPropagation();\n};\ngoog.events.Event.preventDefault = function(e) {\n  e.preventDefault();\n};\n","~:source","// Copyright 2005 The Closure Library Authors. All Rights Reserved.\n//\n// Licensed under the Apache License, Version 2.0 (the \"License\");\n// you may not use this file except in compliance with the License.\n// You may obtain a copy of the License at\n//\n//      http://www.apache.org/licenses/LICENSE-2.0\n//\n// Unless required by applicable law or agreed to in writing, software\n// distributed under the License is distributed on an \"AS-IS\" BASIS,\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n// See the License for the specific language governing permissions and\n// limitations under the License.\n\n/**\n * @fileoverview A base class for event objects.\n *\n */\n\n\ngoog.provide('goog.events.Event');\ngoog.provide('goog.events.EventLike');\n\n/**\n * goog.events.Event no longer depends on goog.Disposable. Keep requiring\n * goog.Disposable here to not break projects which assume this dependency.\n * @suppress {extraRequire}\n */\ngoog.require('goog.Disposable');\ngoog.require('goog.events.EventId');\n\n\n/**\n * A typedef for event like objects that are dispatchable via the\n * goog.events.dispatchEvent function. strings are treated as the type for a\n * goog.events.Event. Objects are treated as an extension of a new\n * goog.events.Event with the type property of the object being used as the type\n * of the Event.\n * @typedef {string|Object|goog.events.Event|goog.events.EventId}\n */\ngoog.events.EventLike;\n\n\n\n/**\n * A base class for event objects, so that they can support preventDefault and\n * stopPropagation.\n *\n * @suppress {underscore} Several properties on this class are technically\n *     public, but referencing these properties outside this package is strongly\n *     discouraged.\n *\n * @param {string|!goog.events.EventId} type Event Type.\n * @param {Object=} opt_target Reference to the object that is the target of\n *     this event. It has to implement the `EventTarget` interface\n *     declared at {@link http://developer.mozilla.org/en/DOM/EventTarget}.\n * @constructor\n */\ngoog.events.Event = function(type, opt_target) {\n  /**\n   * Event type.\n   * @type {string}\n   */\n  this.type = type instanceof goog.events.EventId ? String(type) : type;\n\n  /**\n   * TODO(tbreisacher): The type should probably be\n   * EventTarget|goog.events.EventTarget.\n   *\n   * Target of the event.\n   * @type {Object|undefined}\n   */\n  this.target = opt_target;\n\n  /**\n   * Object that had the listener attached.\n   * @type {Object|undefined}\n   */\n  this.currentTarget = this.target;\n\n  /**\n   * Whether to cancel the event in internal capture/bubble processing for IE.\n   * @type {boolean}\n   * @public\n   */\n  this.propagationStopped_ = false;\n\n  /**\n   * Whether the default action has been prevented.\n   * This is a property to match the W3C specification at\n   * {@link http://www.w3.org/TR/DOM-Level-3-Events/\n   * #events-event-type-defaultPrevented}.\n   * Must be treated as read-only outside the class.\n   * @type {boolean}\n   */\n  this.defaultPrevented = false;\n\n  /**\n   * Return value for in internal capture/bubble processing for IE.\n   * @type {boolean}\n   * @public\n   */\n  this.returnValue_ = true;\n};\n\n\n/**\n * Stops event propagation.\n */\ngoog.events.Event.prototype.stopPropagation = function() {\n  this.propagationStopped_ = true;\n};\n\n\n/**\n * Prevents the default action, for example a link redirecting to a url.\n */\ngoog.events.Event.prototype.preventDefault = function() {\n  this.defaultPrevented = true;\n  this.returnValue_ = false;\n};\n\n\n/**\n * Stops the propagation of the event. It is equivalent to\n * `e.stopPropagation()`, but can be used as the callback argument of\n * {@link goog.events.listen} without declaring another function.\n * @param {!goog.events.Event} e An event.\n */\ngoog.events.Event.stopPropagation = function(e) {\n  e.stopPropagation();\n};\n\n\n/**\n * Prevents the default action. It is equivalent to\n * `e.preventDefault()`, but can be used as the callback argument of\n * {@link goog.events.listen} without declaring another function.\n * @param {!goog.events.Event} e An event.\n */\ngoog.events.Event.preventDefault = function(e) {\n  e.preventDefault();\n};\n","~:compiled-at",1600144468831,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.events.event.js\",\n\"lineCount\":27,\n\"mappings\":\"AAoBAA,IAAA,CAAKC,OAAL,CAAa,mBAAb,CAAA;AACAD,IAAA,CAAKC,OAAL,CAAa,uBAAb,CAAA;AAOAD,IAAA,CAAKE,OAAL,CAAa,iBAAb,CAAA;AACAF,IAAA,CAAKE,OAAL,CAAa,qBAAb,CAAA;AAWAF,IAAA,CAAKG,MAAL,CAAYC,SAAZ;AAkBAJ,IAAA,CAAKG,MAAL,CAAYE,KAAZ,GAAoBC,QAAQ,CAACC,IAAD,EAAOC,UAAP,CAAmB;AAK7C,MAAA,CAAKD,IAAL,GAAYA,IAAA,YAAgBP,IAAhB,CAAqBG,MAArB,CAA4BM,OAA5B,GAAsCC,MAAA,CAAOH,IAAP,CAAtC,GAAqDA,IAAjE;AASA,MAAA,CAAKI,MAAL,GAAcH,UAAd;AAMA,MAAA,CAAKI,aAAL,GAAqB,IAArB,CAA0BD,MAA1B;AAOA,MAAA,CAAKE,mBAAL,GAA2B,KAA3B;AAUA,MAAA,CAAKC,gBAAL,GAAwB,KAAxB;AAOA,MAAA,CAAKC,YAAL,GAAoB,IAApB;AA5C6C,CAA/C;AAmDAf,IAAA,CAAKG,MAAL,CAAYE,KAAZ,CAAkBW,SAAlB,CAA4BC,eAA5B,GAA8CC,QAAQ,EAAG;AACvD,MAAA,CAAKL,mBAAL,GAA2B,IAA3B;AADuD,CAAzD;AAQAb,IAAA,CAAKG,MAAL,CAAYE,KAAZ,CAAkBW,SAAlB,CAA4BG,cAA5B,GAA6CC,QAAQ,EAAG;AACtD,MAAA,CAAKN,gBAAL,GAAwB,IAAxB;AACA,MAAA,CAAKC,YAAL,GAAoB,KAApB;AAFsD,CAAxD;AAYAf,IAAA,CAAKG,MAAL,CAAYE,KAAZ,CAAkBY,eAAlB,GAAoCI,QAAQ,CAACC,CAAD,CAAI;AAC9CA,GAAA,CAAEL,eAAF,EAAA;AAD8C,CAAhD;AAWAjB,IAAA,CAAKG,MAAL,CAAYE,KAAZ,CAAkBc,cAAlB,GAAmCI,QAAQ,CAACD,CAAD,CAAI;AAC7CA,GAAA,CAAEH,cAAF,EAAA;AAD6C,CAA/C;;\",\n\"sources\":[\"goog/events/event.js\"],\n\"sourcesContent\":[\"// Copyright 2005 The Closure Library Authors. All Rights Reserved.\\n//\\n// Licensed under the Apache License, Version 2.0 (the \\\"License\\\");\\n// you may not use this file except in compliance with the License.\\n// You may obtain a copy of the License at\\n//\\n//      http://www.apache.org/licenses/LICENSE-2.0\\n//\\n// Unless required by applicable law or agreed to in writing, software\\n// distributed under the License is distributed on an \\\"AS-IS\\\" BASIS,\\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\\n// See the License for the specific language governing permissions and\\n// limitations under the License.\\n\\n/**\\n * @fileoverview A base class for event objects.\\n *\\n */\\n\\n\\ngoog.provide('goog.events.Event');\\ngoog.provide('goog.events.EventLike');\\n\\n/**\\n * goog.events.Event no longer depends on goog.Disposable. Keep requiring\\n * goog.Disposable here to not break projects which assume this dependency.\\n * @suppress {extraRequire}\\n */\\ngoog.require('goog.Disposable');\\ngoog.require('goog.events.EventId');\\n\\n\\n/**\\n * A typedef for event like objects that are dispatchable via the\\n * goog.events.dispatchEvent function. strings are treated as the type for a\\n * goog.events.Event. Objects are treated as an extension of a new\\n * goog.events.Event with the type property of the object being used as the type\\n * of the Event.\\n * @typedef {string|Object|goog.events.Event|goog.events.EventId}\\n */\\ngoog.events.EventLike;\\n\\n\\n\\n/**\\n * A base class for event objects, so that they can support preventDefault and\\n * stopPropagation.\\n *\\n * @suppress {underscore} Several properties on this class are technically\\n *     public, but referencing these properties outside this package is strongly\\n *     discouraged.\\n *\\n * @param {string|!goog.events.EventId} type Event Type.\\n * @param {Object=} opt_target Reference to the object that is the target of\\n *     this event. It has to implement the `EventTarget` interface\\n *     declared at {@link http://developer.mozilla.org/en/DOM/EventTarget}.\\n * @constructor\\n */\\ngoog.events.Event = function(type, opt_target) {\\n  /**\\n   * Event type.\\n   * @type {string}\\n   */\\n  this.type = type instanceof goog.events.EventId ? String(type) : type;\\n\\n  /**\\n   * TODO(tbreisacher): The type should probably be\\n   * EventTarget|goog.events.EventTarget.\\n   *\\n   * Target of the event.\\n   * @type {Object|undefined}\\n   */\\n  this.target = opt_target;\\n\\n  /**\\n   * Object that had the listener attached.\\n   * @type {Object|undefined}\\n   */\\n  this.currentTarget = this.target;\\n\\n  /**\\n   * Whether to cancel the event in internal capture/bubble processing for IE.\\n   * @type {boolean}\\n   * @public\\n   */\\n  this.propagationStopped_ = false;\\n\\n  /**\\n   * Whether the default action has been prevented.\\n   * This is a property to match the W3C specification at\\n   * {@link http://www.w3.org/TR/DOM-Level-3-Events/\\n   * #events-event-type-defaultPrevented}.\\n   * Must be treated as read-only outside the class.\\n   * @type {boolean}\\n   */\\n  this.defaultPrevented = false;\\n\\n  /**\\n   * Return value for in internal capture/bubble processing for IE.\\n   * @type {boolean}\\n   * @public\\n   */\\n  this.returnValue_ = true;\\n};\\n\\n\\n/**\\n * Stops event propagation.\\n */\\ngoog.events.Event.prototype.stopPropagation = function() {\\n  this.propagationStopped_ = true;\\n};\\n\\n\\n/**\\n * Prevents the default action, for example a link redirecting to a url.\\n */\\ngoog.events.Event.prototype.preventDefault = function() {\\n  this.defaultPrevented = true;\\n  this.returnValue_ = false;\\n};\\n\\n\\n/**\\n * Stops the propagation of the event. It is equivalent to\\n * `e.stopPropagation()`, but can be used as the callback argument of\\n * {@link goog.events.listen} without declaring another function.\\n * @param {!goog.events.Event} e An event.\\n */\\ngoog.events.Event.stopPropagation = function(e) {\\n  e.stopPropagation();\\n};\\n\\n\\n/**\\n * Prevents the default action. It is equivalent to\\n * `e.preventDefault()`, but can be used as the callback argument of\\n * {@link goog.events.listen} without declaring another function.\\n * @param {!goog.events.Event} e An event.\\n */\\ngoog.events.Event.preventDefault = function(e) {\\n  e.preventDefault();\\n};\\n\"],\n\"names\":[\"goog\",\"provide\",\"require\",\"events\",\"EventLike\",\"Event\",\"goog.events.Event\",\"type\",\"opt_target\",\"EventId\",\"String\",\"target\",\"currentTarget\",\"propagationStopped_\",\"defaultPrevented\",\"returnValue_\",\"prototype\",\"stopPropagation\",\"goog.events.Event.prototype.stopPropagation\",\"preventDefault\",\"goog.events.Event.prototype.preventDefault\",\"goog.events.Event.stopPropagation\",\"e\",\"goog.events.Event.preventDefault\"]\n}\n"]