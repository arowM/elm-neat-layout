// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles

(function (modules, entry, mainEntry, parcelRequireName, globalName) {
  /* eslint-disable no-undef */
  var globalObject =
    typeof globalThis !== 'undefined'
      ? globalThis
      : typeof self !== 'undefined'
      ? self
      : typeof window !== 'undefined'
      ? window
      : typeof global !== 'undefined'
      ? global
      : {};
  /* eslint-enable no-undef */

  // Save the require from previous bundle to this closure if any
  var previousRequire =
    typeof globalObject[parcelRequireName] === 'function' &&
    globalObject[parcelRequireName];

  var cache = previousRequire.cache || {};
  // Do not use `require` to prevent Webpack from trying to bundle this call
  var nodeRequire =
    typeof module !== 'undefined' &&
    typeof module.require === 'function' &&
    module.require.bind(module);

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire =
          typeof globalObject[parcelRequireName] === 'function' &&
          globalObject[parcelRequireName];
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error("Cannot find module '" + name + "'");
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = (cache[name] = new newRequire.Module(name));

      modules[name][0].call(
        module.exports,
        localRequire,
        module,
        module.exports,
        this
      );
    }

    return cache[name].exports;

    function localRequire(x) {
      var res = localRequire.resolve(x);
      return res === false ? {} : newRequire(res);
    }

    function resolve(x) {
      var id = modules[name][1][x];
      return id != null ? id : x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [
      function (require, module) {
        module.exports = exports;
      },
      {},
    ];
  };

  Object.defineProperty(newRequire, 'root', {
    get: function () {
      return globalObject[parcelRequireName];
    },
  });

  globalObject[parcelRequireName] = newRequire;

  for (var i = 0; i < entry.length; i++) {
    newRequire(entry[i]);
  }

  if (mainEntry) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(mainEntry);

    // CommonJS
    if (typeof exports === 'object' && typeof module !== 'undefined') {
      module.exports = mainExports;

      // RequireJS
    } else if (typeof define === 'function' && define.amd) {
      define(function () {
        return mainExports;
      });

      // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }
})({"InXGp":[function(require,module,exports) {
var global = arguments[3];
var HMR_HOST = null;
var HMR_PORT = null;
var HMR_SECURE = false;
var HMR_ENV_HASH = "d6ea1d42532a7575";
module.bundle.HMR_BUNDLE_ID = "d6c4bad14ed6b15e";
"use strict";
/* global HMR_HOST, HMR_PORT, HMR_ENV_HASH, HMR_SECURE, chrome, browser, globalThis, __parcel__import__, __parcel__importScripts__, ServiceWorkerGlobalScope */ /*::
import type {
  HMRAsset,
  HMRMessage,
} from '@parcel/reporter-dev-server/src/HMRServer.js';
interface ParcelRequire {
  (string): mixed;
  cache: {|[string]: ParcelModule|};
  hotData: {|[string]: mixed|};
  Module: any;
  parent: ?ParcelRequire;
  isParcelRequire: true;
  modules: {|[string]: [Function, {|[string]: string|}]|};
  HMR_BUNDLE_ID: string;
  root: ParcelRequire;
}
interface ParcelModule {
  hot: {|
    data: mixed,
    accept(cb: (Function) => void): void,
    dispose(cb: (mixed) => void): void,
    // accept(deps: Array<string> | string, cb: (Function) => void): void,
    // decline(): void,
    _acceptCallbacks: Array<(Function) => void>,
    _disposeCallbacks: Array<(mixed) => void>,
  |};
}
interface ExtensionContext {
  runtime: {|
    reload(): void,
    getURL(url: string): string;
    getManifest(): {manifest_version: number, ...};
  |};
}
declare var module: {bundle: ParcelRequire, ...};
declare var HMR_HOST: string;
declare var HMR_PORT: string;
declare var HMR_ENV_HASH: string;
declare var HMR_SECURE: boolean;
declare var chrome: ExtensionContext;
declare var browser: ExtensionContext;
declare var __parcel__import__: (string) => Promise<void>;
declare var __parcel__importScripts__: (string) => Promise<void>;
declare var globalThis: typeof self;
declare var ServiceWorkerGlobalScope: Object;
*/ var OVERLAY_ID = "__parcel__error__overlay__";
var OldModule = module.bundle.Module;
function Module(moduleName) {
    OldModule.call(this, moduleName);
    this.hot = {
        data: module.bundle.hotData[moduleName],
        _acceptCallbacks: [],
        _disposeCallbacks: [],
        accept: function(fn) {
            this._acceptCallbacks.push(fn || function() {});
        },
        dispose: function(fn) {
            this._disposeCallbacks.push(fn);
        }
    };
    module.bundle.hotData[moduleName] = undefined;
}
module.bundle.Module = Module;
module.bundle.hotData = {};
var checkedAssets, assetsToDispose, assetsToAccept /*: Array<[ParcelRequire, string]> */ ;
function getHostname() {
    return HMR_HOST || (location.protocol.indexOf("http") === 0 ? location.hostname : "localhost");
}
function getPort() {
    return HMR_PORT || location.port;
} // eslint-disable-next-line no-redeclare
var parent = module.bundle.parent;
if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== "undefined") {
    var hostname = getHostname();
    var port = getPort();
    var protocol = HMR_SECURE || location.protocol == "https:" && !/localhost|127.0.0.1|0.0.0.0/.test(hostname) ? "wss" : "ws";
    var ws = new WebSocket(protocol + "://" + hostname + (port ? ":" + port : "") + "/"); // Web extension context
    var extCtx = typeof chrome === "undefined" ? typeof browser === "undefined" ? null : browser : chrome; // Safari doesn't support sourceURL in error stacks.
    // eval may also be disabled via CSP, so do a quick check.
    var supportsSourceURL = false;
    try {
        (0, eval)('throw new Error("test"); //# sourceURL=test.js');
    } catch (err) {
        supportsSourceURL = err.stack.includes("test.js");
    } // $FlowFixMe
    ws.onmessage = async function(event) {
        checkedAssets = {} /*: {|[string]: boolean|} */ ;
        assetsToAccept = [];
        assetsToDispose = [];
        var data = JSON.parse(event.data);
        if (data.type === "update") {
            // Remove error overlay if there is one
            if (typeof document !== "undefined") removeErrorOverlay();
            let assets = data.assets.filter((asset)=>asset.envHash === HMR_ENV_HASH); // Handle HMR Update
            let handled = assets.every((asset)=>{
                return asset.type === "css" || asset.type === "js" && hmrAcceptCheck(module.bundle.root, asset.id, asset.depsByBundle);
            });
            if (handled) {
                console.clear(); // Dispatch custom event so other runtimes (e.g React Refresh) are aware.
                if (typeof window !== "undefined" && typeof CustomEvent !== "undefined") window.dispatchEvent(new CustomEvent("parcelhmraccept"));
                await hmrApplyUpdates(assets); // Dispose all old assets.
                let processedAssets = {} /*: {|[string]: boolean|} */ ;
                for(let i = 0; i < assetsToDispose.length; i++){
                    let id = assetsToDispose[i][1];
                    if (!processedAssets[id]) {
                        hmrDispose(assetsToDispose[i][0], id);
                        processedAssets[id] = true;
                    }
                } // Run accept callbacks. This will also re-execute other disposed assets in topological order.
                processedAssets = {};
                for(let i = 0; i < assetsToAccept.length; i++){
                    let id = assetsToAccept[i][1];
                    if (!processedAssets[id]) {
                        hmrAccept(assetsToAccept[i][0], id);
                        processedAssets[id] = true;
                    }
                }
            } else fullReload();
        }
        if (data.type === "error") {
            // Log parcel errors to console
            for (let ansiDiagnostic of data.diagnostics.ansi){
                let stack = ansiDiagnostic.codeframe ? ansiDiagnostic.codeframe : ansiDiagnostic.stack;
                console.error("\uD83D\uDEA8 [parcel]: " + ansiDiagnostic.message + "\n" + stack + "\n\n" + ansiDiagnostic.hints.join("\n"));
            }
            if (typeof document !== "undefined") {
                // Render the fancy html overlay
                removeErrorOverlay();
                var overlay = createErrorOverlay(data.diagnostics.html); // $FlowFixMe
                document.body.appendChild(overlay);
            }
        }
    };
    ws.onerror = function(e) {
        console.error(e.message);
    };
    ws.onclose = function() {
        console.warn("[parcel] \uD83D\uDEA8 Connection to the HMR server was lost");
    };
}
function removeErrorOverlay() {
    var overlay = document.getElementById(OVERLAY_ID);
    if (overlay) {
        overlay.remove();
        console.log("[parcel] ✨ Error resolved");
    }
}
function createErrorOverlay(diagnostics) {
    var overlay = document.createElement("div");
    overlay.id = OVERLAY_ID;
    let errorHTML = '<div style="background: black; opacity: 0.85; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; font-family: Menlo, Consolas, monospace; z-index: 9999;">';
    for (let diagnostic of diagnostics){
        let stack = diagnostic.frames.length ? diagnostic.frames.reduce((p, frame)=>{
            return `${p}
<a href="/__parcel_launch_editor?file=${encodeURIComponent(frame.location)}" style="text-decoration: underline; color: #888" onclick="fetch(this.href); return false">${frame.location}</a>
${frame.code}`;
        }, "") : diagnostic.stack;
        errorHTML += `
      <div>
        <div style="font-size: 18px; font-weight: bold; margin-top: 20px;">
          🚨 ${diagnostic.message}
        </div>
        <pre>${stack}</pre>
        <div>
          ${diagnostic.hints.map((hint)=>"<div>\uD83D\uDCA1 " + hint + "</div>").join("")}
        </div>
        ${diagnostic.documentation ? `<div>📝 <a style="color: violet" href="${diagnostic.documentation}" target="_blank">Learn more</a></div>` : ""}
      </div>
    `;
    }
    errorHTML += "</div>";
    overlay.innerHTML = errorHTML;
    return overlay;
}
function fullReload() {
    if ("reload" in location) location.reload();
    else if (extCtx && extCtx.runtime && extCtx.runtime.reload) extCtx.runtime.reload();
}
function getParents(bundle, id) /*: Array<[ParcelRequire, string]> */ {
    var modules = bundle.modules;
    if (!modules) return [];
    var parents = [];
    var k, d, dep;
    for(k in modules)for(d in modules[k][1]){
        dep = modules[k][1][d];
        if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) parents.push([
            bundle,
            k
        ]);
    }
    if (bundle.parent) parents = parents.concat(getParents(bundle.parent, id));
    return parents;
}
function updateLink(link) {
    var newLink = link.cloneNode();
    newLink.onload = function() {
        if (link.parentNode !== null) // $FlowFixMe
        link.parentNode.removeChild(link);
    };
    newLink.setAttribute("href", link.getAttribute("href").split("?")[0] + "?" + Date.now()); // $FlowFixMe
    link.parentNode.insertBefore(newLink, link.nextSibling);
}
var cssTimeout = null;
function reloadCSS() {
    if (cssTimeout) return;
    cssTimeout = setTimeout(function() {
        var links = document.querySelectorAll('link[rel="stylesheet"]');
        for(var i = 0; i < links.length; i++){
            // $FlowFixMe[incompatible-type]
            var href = links[i].getAttribute("href");
            var hostname = getHostname();
            var servedFromHMRServer = hostname === "localhost" ? new RegExp("^(https?:\\/\\/(0.0.0.0|127.0.0.1)|localhost):" + getPort()).test(href) : href.indexOf(hostname + ":" + getPort());
            var absolute = /^https?:\/\//i.test(href) && href.indexOf(location.origin) !== 0 && !servedFromHMRServer;
            if (!absolute) updateLink(links[i]);
        }
        cssTimeout = null;
    }, 50);
}
function hmrDownload(asset) {
    if (asset.type === "js") {
        if (typeof document !== "undefined") {
            let script = document.createElement("script");
            script.src = asset.url + "?t=" + Date.now();
            if (asset.outputFormat === "esmodule") script.type = "module";
            return new Promise((resolve, reject)=>{
                var _document$head;
                script.onload = ()=>resolve(script);
                script.onerror = reject;
                (_document$head = document.head) === null || _document$head === void 0 || _document$head.appendChild(script);
            });
        } else if (typeof importScripts === "function") {
            // Worker scripts
            if (asset.outputFormat === "esmodule") return import(asset.url + "?t=" + Date.now());
            else return new Promise((resolve, reject)=>{
                try {
                    importScripts(asset.url + "?t=" + Date.now());
                    resolve();
                } catch (err) {
                    reject(err);
                }
            });
        }
    }
}
async function hmrApplyUpdates(assets) {
    global.parcelHotUpdate = Object.create(null);
    let scriptsToRemove;
    try {
        // If sourceURL comments aren't supported in eval, we need to load
        // the update from the dev server over HTTP so that stack traces
        // are correct in errors/logs. This is much slower than eval, so
        // we only do it if needed (currently just Safari).
        // https://bugs.webkit.org/show_bug.cgi?id=137297
        // This path is also taken if a CSP disallows eval.
        if (!supportsSourceURL) {
            let promises = assets.map((asset)=>{
                var _hmrDownload;
                return (_hmrDownload = hmrDownload(asset)) === null || _hmrDownload === void 0 ? void 0 : _hmrDownload.catch((err)=>{
                    // Web extension bugfix for Chromium
                    // https://bugs.chromium.org/p/chromium/issues/detail?id=1255412#c12
                    if (extCtx && extCtx.runtime && extCtx.runtime.getManifest().manifest_version == 3) {
                        if (typeof ServiceWorkerGlobalScope != "undefined" && global instanceof ServiceWorkerGlobalScope) {
                            extCtx.runtime.reload();
                            return;
                        }
                        asset.url = extCtx.runtime.getURL("/__parcel_hmr_proxy__?url=" + encodeURIComponent(asset.url + "?t=" + Date.now()));
                        return hmrDownload(asset);
                    }
                    throw err;
                });
            });
            scriptsToRemove = await Promise.all(promises);
        }
        assets.forEach(function(asset) {
            hmrApply(module.bundle.root, asset);
        });
    } finally{
        delete global.parcelHotUpdate;
        if (scriptsToRemove) scriptsToRemove.forEach((script)=>{
            if (script) {
                var _document$head2;
                (_document$head2 = document.head) === null || _document$head2 === void 0 || _document$head2.removeChild(script);
            }
        });
    }
}
function hmrApply(bundle, asset) {
    var modules = bundle.modules;
    if (!modules) return;
    if (asset.type === "css") reloadCSS();
    else if (asset.type === "js") {
        let deps = asset.depsByBundle[bundle.HMR_BUNDLE_ID];
        if (deps) {
            if (modules[asset.id]) {
                // Remove dependencies that are removed and will become orphaned.
                // This is necessary so that if the asset is added back again, the cache is gone, and we prevent a full page reload.
                let oldDeps = modules[asset.id][1];
                for(let dep in oldDeps)if (!deps[dep] || deps[dep] !== oldDeps[dep]) {
                    let id = oldDeps[dep];
                    let parents = getParents(module.bundle.root, id);
                    if (parents.length === 1) hmrDelete(module.bundle.root, id);
                }
            }
            if (supportsSourceURL) // Global eval. We would use `new Function` here but browser
            // support for source maps is better with eval.
            (0, eval)(asset.output);
             // $FlowFixMe
            let fn = global.parcelHotUpdate[asset.id];
            modules[asset.id] = [
                fn,
                deps
            ];
        } else if (bundle.parent) hmrApply(bundle.parent, asset);
    }
}
function hmrDelete(bundle, id) {
    let modules = bundle.modules;
    if (!modules) return;
    if (modules[id]) {
        // Collect dependencies that will become orphaned when this module is deleted.
        let deps = modules[id][1];
        let orphans = [];
        for(let dep in deps){
            let parents = getParents(module.bundle.root, deps[dep]);
            if (parents.length === 1) orphans.push(deps[dep]);
        } // Delete the module. This must be done before deleting dependencies in case of circular dependencies.
        delete modules[id];
        delete bundle.cache[id]; // Now delete the orphans.
        orphans.forEach((id)=>{
            hmrDelete(module.bundle.root, id);
        });
    } else if (bundle.parent) hmrDelete(bundle.parent, id);
}
function hmrAcceptCheck(bundle, id, depsByBundle) {
    if (hmrAcceptCheckOne(bundle, id, depsByBundle)) return true;
     // Traverse parents breadth first. All possible ancestries must accept the HMR update, or we'll reload.
    let parents = getParents(module.bundle.root, id);
    let accepted = false;
    while(parents.length > 0){
        let v = parents.shift();
        let a = hmrAcceptCheckOne(v[0], v[1], null);
        if (a) // If this parent accepts, stop traversing upward, but still consider siblings.
        accepted = true;
        else {
            // Otherwise, queue the parents in the next level upward.
            let p = getParents(module.bundle.root, v[1]);
            if (p.length === 0) {
                // If there are no parents, then we've reached an entry without accepting. Reload.
                accepted = false;
                break;
            }
            parents.push(...p);
        }
    }
    return accepted;
}
function hmrAcceptCheckOne(bundle, id, depsByBundle) {
    var modules = bundle.modules;
    if (!modules) return;
    if (depsByBundle && !depsByBundle[bundle.HMR_BUNDLE_ID]) {
        // If we reached the root bundle without finding where the asset should go,
        // there's nothing to do. Mark as "accepted" so we don't reload the page.
        if (!bundle.parent) return true;
        return hmrAcceptCheck(bundle.parent, id, depsByBundle);
    }
    if (checkedAssets[id]) return true;
    checkedAssets[id] = true;
    var cached = bundle.cache[id];
    assetsToDispose.push([
        bundle,
        id
    ]);
    if (!cached || cached.hot && cached.hot._acceptCallbacks.length) {
        assetsToAccept.push([
            bundle,
            id
        ]);
        return true;
    }
}
function hmrDispose(bundle, id) {
    var cached = bundle.cache[id];
    bundle.hotData[id] = {};
    if (cached && cached.hot) cached.hot.data = bundle.hotData[id];
    if (cached && cached.hot && cached.hot._disposeCallbacks.length) cached.hot._disposeCallbacks.forEach(function(cb) {
        cb(bundle.hotData[id]);
    });
    delete bundle.cache[id];
}
function hmrAccept(bundle, id) {
    // Execute the module.
    bundle(id); // Run the accept callbacks in the new version of the module.
    var cached = bundle.cache[id];
    if (cached && cached.hot && cached.hot._acceptCallbacks.length) cached.hot._acceptCallbacks.forEach(function(cb) {
        var assetsToAlsoAccept = cb(function() {
            return getParents(module.bundle.root, id);
        });
        if (assetsToAlsoAccept && assetsToAccept.length) {
            assetsToAlsoAccept.forEach(function(a) {
                hmrDispose(a[0], a[1]);
            }); // $FlowFixMe[method-unbinding]
            assetsToAccept.push.apply(assetsToAccept, assetsToAlsoAccept);
        }
    });
}

},{}],"bMtV0":[function(require,module,exports) {
const { Elm  } = require("c779e5def2024fd2");
const app = Elm.Main.init({
    node: document.body.appendChild(document.createElement("div")),
    flags: null
});

},{"c779e5def2024fd2":"jtLtl"}],"jtLtl":[function(require,module,exports) {
(function(scope) {
    "use strict";
    function F(arity, fun, wrapper) {
        wrapper.a = arity;
        wrapper.f = fun;
        return wrapper;
    }
    function F2(fun) {
        return F(2, fun, function(a) {
            return function(b) {
                return fun(a, b);
            };
        });
    }
    function F3(fun) {
        return F(3, fun, function(a) {
            return function(b) {
                return function(c) {
                    return fun(a, b, c);
                };
            };
        });
    }
    function F4(fun) {
        return F(4, fun, function(a) {
            return function(b) {
                return function(c) {
                    return function(d) {
                        return fun(a, b, c, d);
                    };
                };
            };
        });
    }
    function F5(fun) {
        return F(5, fun, function(a) {
            return function(b) {
                return function(c) {
                    return function(d) {
                        return function(e) {
                            return fun(a, b, c, d, e);
                        };
                    };
                };
            };
        });
    }
    function F6(fun) {
        return F(6, fun, function(a) {
            return function(b) {
                return function(c) {
                    return function(d) {
                        return function(e) {
                            return function(f) {
                                return fun(a, b, c, d, e, f);
                            };
                        };
                    };
                };
            };
        });
    }
    function F7(fun) {
        return F(7, fun, function(a) {
            return function(b) {
                return function(c) {
                    return function(d) {
                        return function(e) {
                            return function(f) {
                                return function(g) {
                                    return fun(a, b, c, d, e, f, g);
                                };
                            };
                        };
                    };
                };
            };
        });
    }
    function F8(fun) {
        return F(8, fun, function(a) {
            return function(b) {
                return function(c) {
                    return function(d) {
                        return function(e) {
                            return function(f) {
                                return function(g) {
                                    return function(h) {
                                        return fun(a, b, c, d, e, f, g, h);
                                    };
                                };
                            };
                        };
                    };
                };
            };
        });
    }
    function F9(fun) {
        return F(9, fun, function(a) {
            return function(b) {
                return function(c) {
                    return function(d) {
                        return function(e) {
                            return function(f) {
                                return function(g) {
                                    return function(h) {
                                        return function(i) {
                                            return fun(a, b, c, d, e, f, g, h, i);
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        });
    }
    function A2(fun, a, b) {
        return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
    }
    function A3(fun, a, b, c) {
        return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
    }
    function A4(fun, a, b, c, d) {
        return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
    }
    function A5(fun, a, b, c, d, e) {
        return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
    }
    function A6(fun, a, b, c, d, e, f) {
        return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
    }
    function A7(fun, a, b, c, d, e, f, g) {
        return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
    }
    function A8(fun, a, b, c, d, e, f, g, h) {
        return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
    }
    function A9(fun, a, b, c, d, e, f, g, h, i) {
        return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
    }
    console.warn("Compiled in DEBUG mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.");
    // EQUALITY
    function _Utils_eq(x, y) {
        for(var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack); isEqual && (pair = stack.pop()); isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack));
        return isEqual;
    }
    function _Utils_eqHelp(x, y, depth, stack) {
        if (depth > 100) {
            stack.push(_Utils_Tuple2(x, y));
            return true;
        }
        if (x === y) return true;
        if (typeof x !== "object" || x === null || y === null) {
            typeof x === "function" && _Debug_crash(5);
            return false;
        }
        /**/ if (x.$ === "Set_elm_builtin") {
            x = $elm$core$Set$toList(x);
            y = $elm$core$Set$toList(y);
        }
        if (x.$ === "RBNode_elm_builtin" || x.$ === "RBEmpty_elm_builtin") {
            x = $elm$core$Dict$toList(x);
            y = $elm$core$Dict$toList(y);
        }
        //*/
        /**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/ for(var key in x){
            if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack)) return false;
        }
        return true;
    }
    var _Utils_equal = F2(_Utils_eq);
    var _Utils_notEqual = F2(function(a, b) {
        return !_Utils_eq(a, b);
    });
    // COMPARISONS
    // Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
    // the particular integer values assigned to LT, EQ, and GT.
    function _Utils_cmp(x, y, ord) {
        if (typeof x !== "object") return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
        /**/ if (x instanceof String) {
            var a = x.valueOf();
            var b = y.valueOf();
            return a === b ? 0 : a < b ? -1 : 1;
        }
        //*/
        /**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/ /**/ if (x.$[0] === "#") return (ord = _Utils_cmp(x.a, y.a)) ? ord : (ord = _Utils_cmp(x.b, y.b)) ? ord : _Utils_cmp(x.c, y.c);
        // traverse conses until end of a list or a mismatch
        for(; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b); // WHILE_CONSES
        return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
    }
    var _Utils_lt = F2(function(a, b) {
        return _Utils_cmp(a, b) < 0;
    });
    var _Utils_le = F2(function(a, b) {
        return _Utils_cmp(a, b) < 1;
    });
    var _Utils_gt = F2(function(a, b) {
        return _Utils_cmp(a, b) > 0;
    });
    var _Utils_ge = F2(function(a, b) {
        return _Utils_cmp(a, b) >= 0;
    });
    var _Utils_compare = F2(function(x, y) {
        var n = _Utils_cmp(x, y);
        return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
    });
    // COMMON VALUES
    var _Utils_Tuple0_UNUSED = 0;
    var _Utils_Tuple0 = {
        $: "#0"
    };
    function _Utils_Tuple2_UNUSED(a, b) {
        return {
            a: a,
            b: b
        };
    }
    function _Utils_Tuple2(a, b) {
        return {
            $: "#2",
            a: a,
            b: b
        };
    }
    function _Utils_Tuple3_UNUSED(a, b, c) {
        return {
            a: a,
            b: b,
            c: c
        };
    }
    function _Utils_Tuple3(a, b, c) {
        return {
            $: "#3",
            a: a,
            b: b,
            c: c
        };
    }
    function _Utils_chr_UNUSED(c) {
        return c;
    }
    function _Utils_chr(c) {
        return new String(c);
    }
    // RECORDS
    function _Utils_update(oldRecord, updatedFields) {
        var newRecord = {};
        for(var key in oldRecord)newRecord[key] = oldRecord[key];
        for(var key in updatedFields)newRecord[key] = updatedFields[key];
        return newRecord;
    }
    // APPEND
    var _Utils_append = F2(_Utils_ap);
    function _Utils_ap(xs, ys) {
        // append Strings
        if (typeof xs === "string") return xs + ys;
        // append Lists
        if (!xs.b) return ys;
        var root = _List_Cons(xs.a, ys);
        xs = xs.b;
        for(var curr = root; xs.b; xs = xs.b)curr = curr.b = _List_Cons(xs.a, ys);
        return root;
    }
    var _List_Nil_UNUSED = {
        $: 0
    };
    var _List_Nil = {
        $: "[]"
    };
    function _List_Cons_UNUSED(hd, tl) {
        return {
            $: 1,
            a: hd,
            b: tl
        };
    }
    function _List_Cons(hd, tl) {
        return {
            $: "::",
            a: hd,
            b: tl
        };
    }
    var _List_cons = F2(_List_Cons);
    function _List_fromArray(arr) {
        var out = _List_Nil;
        for(var i = arr.length; i--;)out = _List_Cons(arr[i], out);
        return out;
    }
    function _List_toArray(xs) {
        for(var out = []; xs.b; xs = xs.b)out.push(xs.a);
        return out;
    }
    var _List_map2 = F3(function(f, xs, ys) {
        for(var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b)arr.push(A2(f, xs.a, ys.a));
        return _List_fromArray(arr);
    });
    var _List_map3 = F4(function(f, xs, ys, zs) {
        for(var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b)arr.push(A3(f, xs.a, ys.a, zs.a));
        return _List_fromArray(arr);
    });
    var _List_map4 = F5(function(f, ws, xs, ys, zs) {
        for(var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b)arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
        return _List_fromArray(arr);
    });
    var _List_map5 = F6(function(f, vs, ws, xs, ys, zs) {
        for(var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b)arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
        return _List_fromArray(arr);
    });
    var _List_sortBy = F2(function(f, xs) {
        return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
            return _Utils_cmp(f(a), f(b));
        }));
    });
    var _List_sortWith = F2(function(f, xs) {
        return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
            var ord = A2(f, a, b);
            return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
        }));
    });
    var _JsArray_empty = [];
    function _JsArray_singleton(value) {
        return [
            value
        ];
    }
    function _JsArray_length(array) {
        return array.length;
    }
    var _JsArray_initialize = F3(function(size, offset, func) {
        var result = new Array(size);
        for(var i = 0; i < size; i++)result[i] = func(offset + i);
        return result;
    });
    var _JsArray_initializeFromList = F2(function(max, ls) {
        var result = new Array(max);
        for(var i = 0; i < max && ls.b; i++){
            result[i] = ls.a;
            ls = ls.b;
        }
        result.length = i;
        return _Utils_Tuple2(result, ls);
    });
    var _JsArray_unsafeGet = F2(function(index, array) {
        return array[index];
    });
    var _JsArray_unsafeSet = F3(function(index, value, array) {
        var length = array.length;
        var result = new Array(length);
        for(var i = 0; i < length; i++)result[i] = array[i];
        result[index] = value;
        return result;
    });
    var _JsArray_push = F2(function(value, array) {
        var length = array.length;
        var result = new Array(length + 1);
        for(var i = 0; i < length; i++)result[i] = array[i];
        result[length] = value;
        return result;
    });
    var _JsArray_foldl = F3(function(func, acc, array) {
        var length = array.length;
        for(var i = 0; i < length; i++)acc = A2(func, array[i], acc);
        return acc;
    });
    var _JsArray_foldr = F3(function(func, acc, array) {
        for(var i = array.length - 1; i >= 0; i--)acc = A2(func, array[i], acc);
        return acc;
    });
    var _JsArray_map = F2(function(func, array) {
        var length = array.length;
        var result = new Array(length);
        for(var i = 0; i < length; i++)result[i] = func(array[i]);
        return result;
    });
    var _JsArray_indexedMap = F3(function(func, offset, array) {
        var length = array.length;
        var result = new Array(length);
        for(var i = 0; i < length; i++)result[i] = A2(func, offset + i, array[i]);
        return result;
    });
    var _JsArray_slice = F3(function(from, to, array) {
        return array.slice(from, to);
    });
    var _JsArray_appendN = F3(function(n, dest, source) {
        var destLen = dest.length;
        var itemsToCopy = n - destLen;
        if (itemsToCopy > source.length) itemsToCopy = source.length;
        var size = destLen + itemsToCopy;
        var result = new Array(size);
        for(var i = 0; i < destLen; i++)result[i] = dest[i];
        for(var i = 0; i < itemsToCopy; i++)result[i + destLen] = source[i];
        return result;
    });
    // LOG
    var _Debug_log_UNUSED = F2(function(tag, value) {
        return value;
    });
    var _Debug_log = F2(function(tag, value) {
        console.log(tag + ": " + _Debug_toString(value));
        return value;
    });
    // TODOS
    function _Debug_todo(moduleName, region) {
        return function(message) {
            _Debug_crash(8, moduleName, region, message);
        };
    }
    function _Debug_todoCase(moduleName, region, value) {
        return function(message) {
            _Debug_crash(9, moduleName, region, value, message);
        };
    }
    // TO STRING
    function _Debug_toString_UNUSED(value) {
        return "<internals>";
    }
    function _Debug_toString(value) {
        return _Debug_toAnsiString(false, value);
    }
    function _Debug_toAnsiString(ansi, value) {
        if (typeof value === "function") return _Debug_internalColor(ansi, "<function>");
        if (typeof value === "boolean") return _Debug_ctorColor(ansi, value ? "True" : "False");
        if (typeof value === "number") return _Debug_numberColor(ansi, value + "");
        if (value instanceof String) return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
        if (typeof value === "string") return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
        if (typeof value === "object" && "$" in value) {
            var tag = value.$;
            if (typeof tag === "number") return _Debug_internalColor(ansi, "<internals>");
            if (tag[0] === "#") {
                var output = [];
                for(var k in value){
                    if (k === "$") continue;
                    output.push(_Debug_toAnsiString(ansi, value[k]));
                }
                return "(" + output.join(",") + ")";
            }
            if (tag === "Set_elm_builtin") return _Debug_ctorColor(ansi, "Set") + _Debug_fadeColor(ansi, ".fromList") + " " + _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
            if (tag === "RBNode_elm_builtin" || tag === "RBEmpty_elm_builtin") return _Debug_ctorColor(ansi, "Dict") + _Debug_fadeColor(ansi, ".fromList") + " " + _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
            if (tag === "Array_elm_builtin") return _Debug_ctorColor(ansi, "Array") + _Debug_fadeColor(ansi, ".fromList") + " " + _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
            if (tag === "::" || tag === "[]") {
                var output = "[";
                value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b);
                for(; value.b; value = value.b)output += "," + _Debug_toAnsiString(ansi, value.a);
                return output + "]";
            }
            var output = "";
            for(var i in value){
                if (i === "$") continue;
                var str = _Debug_toAnsiString(ansi, value[i]);
                var c0 = str[0];
                var parenless = c0 === "{" || c0 === "(" || c0 === "[" || c0 === "<" || c0 === '"' || str.indexOf(" ") < 0;
                output += " " + (parenless ? str : "(" + str + ")");
            }
            return _Debug_ctorColor(ansi, tag) + output;
        }
        if (typeof DataView === "function" && value instanceof DataView) return _Debug_stringColor(ansi, "<" + value.byteLength + " bytes>");
        if (typeof File === "function" && value instanceof File) return _Debug_internalColor(ansi, "<" + value.name + ">");
        if (typeof value === "object") {
            var output = [];
            for(var key in value){
                var field = key[0] === "_" ? key.slice(1) : key;
                output.push(_Debug_fadeColor(ansi, field) + " = " + _Debug_toAnsiString(ansi, value[key]));
            }
            if (output.length === 0) return "{}";
            return "{ " + output.join(", ") + " }";
        }
        return _Debug_internalColor(ansi, "<internals>");
    }
    function _Debug_addSlashes(str, isChar) {
        var s = str.replace(/\\/g, "\\\\").replace(/\n/g, "\\n").replace(/\t/g, "\\t").replace(/\r/g, "\\r").replace(/\v/g, "\\v").replace(/\0/g, "\\0");
        if (isChar) return s.replace(/\'/g, "\\'");
        else return s.replace(/\"/g, '\\"');
    }
    function _Debug_ctorColor(ansi, string) {
        return ansi ? "\x1b[96m" + string + "\x1b[0m" : string;
    }
    function _Debug_numberColor(ansi, string) {
        return ansi ? "\x1b[95m" + string + "\x1b[0m" : string;
    }
    function _Debug_stringColor(ansi, string) {
        return ansi ? "\x1b[93m" + string + "\x1b[0m" : string;
    }
    function _Debug_charColor(ansi, string) {
        return ansi ? "\x1b[92m" + string + "\x1b[0m" : string;
    }
    function _Debug_fadeColor(ansi, string) {
        return ansi ? "\x1b[37m" + string + "\x1b[0m" : string;
    }
    function _Debug_internalColor(ansi, string) {
        return ansi ? "\x1b[94m" + string + "\x1b[0m" : string;
    }
    function _Debug_toHexDigit(n) {
        return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
    }
    // CRASH
    function _Debug_crash_UNUSED(identifier) {
        throw new Error("https://github.com/elm/core/blob/1.0.0/hints/" + identifier + ".md");
    }
    function _Debug_crash(identifier, fact1, fact2, fact3, fact4) {
        switch(identifier){
            case 0:
                throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');
            case 1:
                throw new Error("Browser.application programs cannot handle URLs like this:\n\n    " + document.location.href + "\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.");
            case 2:
                var jsonErrorString = fact1;
                throw new Error("Problem with the flags given to your Elm program on initialization.\n\n" + jsonErrorString);
            case 3:
                var portName = fact1;
                throw new Error("There can only be one port named `" + portName + "`, but your program has multiple.");
            case 4:
                var portName = fact1;
                var problem = fact2;
                throw new Error("Trying to send an unexpected type of value through port `" + portName + "`:\n" + problem);
            case 5:
                throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');
            case 6:
                var moduleName = fact1;
                throw new Error("Your page is loading multiple Elm scripts with a module named " + moduleName + ". Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!");
            case 8:
                var moduleName = fact1;
                var region = fact2;
                var message = fact3;
                throw new Error("TODO in module `" + moduleName + "` " + _Debug_regionToString(region) + "\n\n" + message);
            case 9:
                var moduleName = fact1;
                var region = fact2;
                var value = fact3;
                var message = fact4;
                throw new Error("TODO in module `" + moduleName + "` from the `case` expression " + _Debug_regionToString(region) + "\n\nIt received the following value:\n\n    " + _Debug_toString(value).replace("\n", "\n    ") + "\n\nBut the branch that handles it says:\n\n    " + message.replace("\n", "\n    "));
            case 10:
                throw new Error("Bug in https://github.com/elm/virtual-dom/issues");
            case 11:
                throw new Error("Cannot perform mod 0. Division by zero error.");
        }
    }
    function _Debug_regionToString(region) {
        if (region.start.line === region.end.line) return "on line " + region.start.line;
        return "on lines " + region.start.line + " through " + region.end.line;
    }
    // MATH
    var _Basics_add = F2(function(a, b) {
        return a + b;
    });
    var _Basics_sub = F2(function(a, b) {
        return a - b;
    });
    var _Basics_mul = F2(function(a, b) {
        return a * b;
    });
    var _Basics_fdiv = F2(function(a, b) {
        return a / b;
    });
    var _Basics_idiv = F2(function(a, b) {
        return a / b | 0;
    });
    var _Basics_pow = F2(Math.pow);
    var _Basics_remainderBy = F2(function(b, a) {
        return a % b;
    });
    // https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
    var _Basics_modBy = F2(function(modulus, x) {
        var answer = x % modulus;
        return modulus === 0 ? _Debug_crash(11) : answer > 0 && modulus < 0 || answer < 0 && modulus > 0 ? answer + modulus : answer;
    });
    // TRIGONOMETRY
    var _Basics_pi = Math.PI;
    var _Basics_e = Math.E;
    var _Basics_cos = Math.cos;
    var _Basics_sin = Math.sin;
    var _Basics_tan = Math.tan;
    var _Basics_acos = Math.acos;
    var _Basics_asin = Math.asin;
    var _Basics_atan = Math.atan;
    var _Basics_atan2 = F2(Math.atan2);
    // MORE MATH
    function _Basics_toFloat(x) {
        return x;
    }
    function _Basics_truncate(n) {
        return n | 0;
    }
    function _Basics_isInfinite(n) {
        return n === Infinity || n === -Infinity;
    }
    var _Basics_ceiling = Math.ceil;
    var _Basics_floor = Math.floor;
    var _Basics_round = Math.round;
    var _Basics_sqrt = Math.sqrt;
    var _Basics_log = Math.log;
    var _Basics_isNaN = isNaN;
    // BOOLEANS
    function _Basics_not(bool) {
        return !bool;
    }
    var _Basics_and = F2(function(a, b) {
        return a && b;
    });
    var _Basics_or = F2(function(a, b) {
        return a || b;
    });
    var _Basics_xor = F2(function(a, b) {
        return a !== b;
    });
    var _String_cons = F2(function(chr, str) {
        return chr + str;
    });
    function _String_uncons(string) {
        var word = string.charCodeAt(0);
        return word ? $elm$core$Maybe$Just(0xD800 <= word && word <= 0xDBFF ? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2)) : _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))) : $elm$core$Maybe$Nothing;
    }
    var _String_append = F2(function(a, b) {
        return a + b;
    });
    function _String_length(str) {
        return str.length;
    }
    var _String_map = F2(function(func, string) {
        var len = string.length;
        var array = new Array(len);
        var i = 0;
        while(i < len){
            var word = string.charCodeAt(i);
            if (0xD800 <= word && word <= 0xDBFF) {
                array[i] = func(_Utils_chr(string[i] + string[i + 1]));
                i += 2;
                continue;
            }
            array[i] = func(_Utils_chr(string[i]));
            i++;
        }
        return array.join("");
    });
    var _String_filter = F2(function(isGood, str) {
        var arr = [];
        var len = str.length;
        var i = 0;
        while(i < len){
            var char = str[i];
            var word = str.charCodeAt(i);
            i++;
            if (0xD800 <= word && word <= 0xDBFF) {
                char += str[i];
                i++;
            }
            if (isGood(_Utils_chr(char))) arr.push(char);
        }
        return arr.join("");
    });
    function _String_reverse(str) {
        var len = str.length;
        var arr = new Array(len);
        var i = 0;
        while(i < len){
            var word = str.charCodeAt(i);
            if (0xD800 <= word && word <= 0xDBFF) {
                arr[len - i] = str[i + 1];
                i++;
                arr[len - i] = str[i - 1];
                i++;
            } else {
                arr[len - i] = str[i];
                i++;
            }
        }
        return arr.join("");
    }
    var _String_foldl = F3(function(func, state, string) {
        var len = string.length;
        var i = 0;
        while(i < len){
            var char = string[i];
            var word = string.charCodeAt(i);
            i++;
            if (0xD800 <= word && word <= 0xDBFF) {
                char += string[i];
                i++;
            }
            state = A2(func, _Utils_chr(char), state);
        }
        return state;
    });
    var _String_foldr = F3(function(func, state, string) {
        var i = string.length;
        while(i--){
            var char = string[i];
            var word = string.charCodeAt(i);
            if (0xDC00 <= word && word <= 0xDFFF) {
                i--;
                char = string[i] + char;
            }
            state = A2(func, _Utils_chr(char), state);
        }
        return state;
    });
    var _String_split = F2(function(sep, str) {
        return str.split(sep);
    });
    var _String_join = F2(function(sep, strs) {
        return strs.join(sep);
    });
    var _String_slice = F3(function(start, end, str) {
        return str.slice(start, end);
    });
    function _String_trim(str) {
        return str.trim();
    }
    function _String_trimLeft(str) {
        return str.replace(/^\s+/, "");
    }
    function _String_trimRight(str) {
        return str.replace(/\s+$/, "");
    }
    function _String_words(str) {
        return _List_fromArray(str.trim().split(/\s+/g));
    }
    function _String_lines(str) {
        return _List_fromArray(str.split(/\r\n|\r|\n/g));
    }
    function _String_toUpper(str) {
        return str.toUpperCase();
    }
    function _String_toLower(str) {
        return str.toLowerCase();
    }
    var _String_any = F2(function(isGood, string) {
        var i = string.length;
        while(i--){
            var char = string[i];
            var word = string.charCodeAt(i);
            if (0xDC00 <= word && word <= 0xDFFF) {
                i--;
                char = string[i] + char;
            }
            if (isGood(_Utils_chr(char))) return true;
        }
        return false;
    });
    var _String_all = F2(function(isGood, string) {
        var i = string.length;
        while(i--){
            var char = string[i];
            var word = string.charCodeAt(i);
            if (0xDC00 <= word && word <= 0xDFFF) {
                i--;
                char = string[i] + char;
            }
            if (!isGood(_Utils_chr(char))) return false;
        }
        return true;
    });
    var _String_contains = F2(function(sub, str) {
        return str.indexOf(sub) > -1;
    });
    var _String_startsWith = F2(function(sub, str) {
        return str.indexOf(sub) === 0;
    });
    var _String_endsWith = F2(function(sub, str) {
        return str.length >= sub.length && str.lastIndexOf(sub) === str.length - sub.length;
    });
    var _String_indexes = F2(function(sub, str) {
        var subLen = sub.length;
        if (subLen < 1) return _List_Nil;
        var i = 0;
        var is = [];
        while((i = str.indexOf(sub, i)) > -1){
            is.push(i);
            i = i + subLen;
        }
        return _List_fromArray(is);
    });
    // TO STRING
    function _String_fromNumber(number) {
        return number + "";
    }
    // INT CONVERSIONS
    function _String_toInt(str) {
        var total = 0;
        var code0 = str.charCodeAt(0);
        var start = code0 == 0x2B /* + */  || code0 == 0x2D /* - */  ? 1 : 0;
        for(var i = start; i < str.length; ++i){
            var code = str.charCodeAt(i);
            if (code < 0x30 || 0x39 < code) return $elm$core$Maybe$Nothing;
            total = 10 * total + code - 0x30;
        }
        return i == start ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
    }
    // FLOAT CONVERSIONS
    function _String_toFloat(s) {
        // check if it is a hex, octal, or binary number
        if (s.length === 0 || /[\sxbo]/.test(s)) return $elm$core$Maybe$Nothing;
        var n = +s;
        // faster isNaN check
        return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
    }
    function _String_fromList(chars) {
        return _List_toArray(chars).join("");
    }
    function _Char_toCode(char) {
        var code = char.charCodeAt(0);
        if (0xD800 <= code && code <= 0xDBFF) return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000;
        return code;
    }
    function _Char_fromCode(code) {
        return _Utils_chr(code < 0 || 0x10FFFF < code ? "�" : code <= 0xFFFF ? String.fromCharCode(code) : (code -= 0x10000, String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)));
    }
    function _Char_toUpper(char) {
        return _Utils_chr(char.toUpperCase());
    }
    function _Char_toLower(char) {
        return _Utils_chr(char.toLowerCase());
    }
    function _Char_toLocaleUpper(char) {
        return _Utils_chr(char.toLocaleUpperCase());
    }
    function _Char_toLocaleLower(char) {
        return _Utils_chr(char.toLocaleLowerCase());
    }
    /**/ function _Json_errorToString(error) {
        return $elm$json$Json$Decode$errorToString(error);
    }
    //*/
    // CORE DECODERS
    function _Json_succeed(msg) {
        return {
            $: 0,
            a: msg
        };
    }
    function _Json_fail(msg) {
        return {
            $: 1,
            a: msg
        };
    }
    function _Json_decodePrim(decoder) {
        return {
            $: 2,
            b: decoder
        };
    }
    var _Json_decodeInt = _Json_decodePrim(function(value) {
        return typeof value !== "number" ? _Json_expecting("an INT", value) : -2147483647 < value && value < 2147483647 && (value | 0) === value ? $elm$core$Result$Ok(value) : isFinite(value) && !(value % 1) ? $elm$core$Result$Ok(value) : _Json_expecting("an INT", value);
    });
    var _Json_decodeBool = _Json_decodePrim(function(value) {
        return typeof value === "boolean" ? $elm$core$Result$Ok(value) : _Json_expecting("a BOOL", value);
    });
    var _Json_decodeFloat = _Json_decodePrim(function(value) {
        return typeof value === "number" ? $elm$core$Result$Ok(value) : _Json_expecting("a FLOAT", value);
    });
    var _Json_decodeValue = _Json_decodePrim(function(value) {
        return $elm$core$Result$Ok(_Json_wrap(value));
    });
    var _Json_decodeString = _Json_decodePrim(function(value) {
        return typeof value === "string" ? $elm$core$Result$Ok(value) : value instanceof String ? $elm$core$Result$Ok(value + "") : _Json_expecting("a STRING", value);
    });
    function _Json_decodeList(decoder) {
        return {
            $: 3,
            b: decoder
        };
    }
    function _Json_decodeArray(decoder) {
        return {
            $: 4,
            b: decoder
        };
    }
    function _Json_decodeNull(value) {
        return {
            $: 5,
            c: value
        };
    }
    var _Json_decodeField = F2(function(field, decoder) {
        return {
            $: 6,
            d: field,
            b: decoder
        };
    });
    var _Json_decodeIndex = F2(function(index, decoder) {
        return {
            $: 7,
            e: index,
            b: decoder
        };
    });
    function _Json_decodeKeyValuePairs(decoder) {
        return {
            $: 8,
            b: decoder
        };
    }
    function _Json_mapMany(f, decoders) {
        return {
            $: 9,
            f: f,
            g: decoders
        };
    }
    var _Json_andThen = F2(function(callback, decoder) {
        return {
            $: 10,
            b: decoder,
            h: callback
        };
    });
    function _Json_oneOf(decoders) {
        return {
            $: 11,
            g: decoders
        };
    }
    // DECODING OBJECTS
    var _Json_map1 = F2(function(f, d1) {
        return _Json_mapMany(f, [
            d1
        ]);
    });
    var _Json_map2 = F3(function(f, d1, d2) {
        return _Json_mapMany(f, [
            d1,
            d2
        ]);
    });
    var _Json_map3 = F4(function(f, d1, d2, d3) {
        return _Json_mapMany(f, [
            d1,
            d2,
            d3
        ]);
    });
    var _Json_map4 = F5(function(f, d1, d2, d3, d4) {
        return _Json_mapMany(f, [
            d1,
            d2,
            d3,
            d4
        ]);
    });
    var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5) {
        return _Json_mapMany(f, [
            d1,
            d2,
            d3,
            d4,
            d5
        ]);
    });
    var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6) {
        return _Json_mapMany(f, [
            d1,
            d2,
            d3,
            d4,
            d5,
            d6
        ]);
    });
    var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7) {
        return _Json_mapMany(f, [
            d1,
            d2,
            d3,
            d4,
            d5,
            d6,
            d7
        ]);
    });
    var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8) {
        return _Json_mapMany(f, [
            d1,
            d2,
            d3,
            d4,
            d5,
            d6,
            d7,
            d8
        ]);
    });
    // DECODE
    var _Json_runOnString = F2(function(decoder, string) {
        try {
            var value = JSON.parse(string);
            return _Json_runHelp(decoder, value);
        } catch (e) {
            return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, "This is not valid JSON! " + e.message, _Json_wrap(string)));
        }
    });
    var _Json_run = F2(function(decoder, value) {
        return _Json_runHelp(decoder, _Json_unwrap(value));
    });
    function _Json_runHelp(decoder, value) {
        switch(decoder.$){
            case 2:
                return decoder.b(value);
            case 5:
                return value === null ? $elm$core$Result$Ok(decoder.c) : _Json_expecting("null", value);
            case 3:
                if (!_Json_isArray(value)) return _Json_expecting("a LIST", value);
                return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);
            case 4:
                if (!_Json_isArray(value)) return _Json_expecting("an ARRAY", value);
                return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);
            case 6:
                var field = decoder.d;
                if (typeof value !== "object" || value === null || !(field in value)) return _Json_expecting("an OBJECT with a field named `" + field + "`", value);
                var result = _Json_runHelp(decoder.b, value[field]);
                return $elm$core$Result$isOk(result) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));
            case 7:
                var index = decoder.e;
                if (!_Json_isArray(value)) return _Json_expecting("an ARRAY", value);
                if (index >= value.length) return _Json_expecting("a LONGER array. Need index " + index + " but only see " + value.length + " entries", value);
                var result = _Json_runHelp(decoder.b, value[index]);
                return $elm$core$Result$isOk(result) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));
            case 8:
                if (typeof value !== "object" || value === null || _Json_isArray(value)) return _Json_expecting("an OBJECT", value);
                var keyValuePairs = _List_Nil;
                // TODO test perf of Object.keys and switch when support is good enough
                for(var key in value)if (value.hasOwnProperty(key)) {
                    var result = _Json_runHelp(decoder.b, value[key]);
                    if (!$elm$core$Result$isOk(result)) return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
                    keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
                }
                return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));
            case 9:
                var answer = decoder.f;
                var decoders = decoder.g;
                for(var i = 0; i < decoders.length; i++){
                    var result = _Json_runHelp(decoders[i], value);
                    if (!$elm$core$Result$isOk(result)) return result;
                    answer = answer(result.a);
                }
                return $elm$core$Result$Ok(answer);
            case 10:
                var result = _Json_runHelp(decoder.b, value);
                return !$elm$core$Result$isOk(result) ? result : _Json_runHelp(decoder.h(result.a), value);
            case 11:
                var errors = _List_Nil;
                for(var temp = decoder.g; temp.b; temp = temp.b){
                    var result = _Json_runHelp(temp.a, value);
                    if ($elm$core$Result$isOk(result)) return result;
                    errors = _List_Cons(result.a, errors);
                }
                return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));
            case 1:
                return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));
            case 0:
                return $elm$core$Result$Ok(decoder.a);
        }
    }
    function _Json_runArrayDecoder(decoder, value, toElmValue) {
        var len = value.length;
        var array = new Array(len);
        for(var i = 0; i < len; i++){
            var result = _Json_runHelp(decoder, value[i]);
            if (!$elm$core$Result$isOk(result)) return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
            array[i] = result.a;
        }
        return $elm$core$Result$Ok(toElmValue(array));
    }
    function _Json_isArray(value) {
        return Array.isArray(value) || typeof FileList !== "undefined" && value instanceof FileList;
    }
    function _Json_toElmArray(array) {
        return A2($elm$core$Array$initialize, array.length, function(i) {
            return array[i];
        });
    }
    function _Json_expecting(type, value) {
        return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, "Expecting " + type, _Json_wrap(value)));
    }
    // EQUALITY
    function _Json_equality(x, y) {
        if (x === y) return true;
        if (x.$ !== y.$) return false;
        switch(x.$){
            case 0:
            case 1:
                return x.a === y.a;
            case 2:
                return x.b === y.b;
            case 5:
                return x.c === y.c;
            case 3:
            case 4:
            case 8:
                return _Json_equality(x.b, y.b);
            case 6:
                return x.d === y.d && _Json_equality(x.b, y.b);
            case 7:
                return x.e === y.e && _Json_equality(x.b, y.b);
            case 9:
                return x.f === y.f && _Json_listEquality(x.g, y.g);
            case 10:
                return x.h === y.h && _Json_equality(x.b, y.b);
            case 11:
                return _Json_listEquality(x.g, y.g);
        }
    }
    function _Json_listEquality(aDecoders, bDecoders) {
        var len = aDecoders.length;
        if (len !== bDecoders.length) return false;
        for(var i = 0; i < len; i++){
            if (!_Json_equality(aDecoders[i], bDecoders[i])) return false;
        }
        return true;
    }
    // ENCODE
    var _Json_encode = F2(function(indentLevel, value) {
        return JSON.stringify(_Json_unwrap(value), null, indentLevel) + "";
    });
    function _Json_wrap(value) {
        return {
            $: 0,
            a: value
        };
    }
    function _Json_unwrap(value) {
        return value.a;
    }
    function _Json_wrap_UNUSED(value) {
        return value;
    }
    function _Json_unwrap_UNUSED(value) {
        return value;
    }
    function _Json_emptyArray() {
        return [];
    }
    function _Json_emptyObject() {
        return {};
    }
    var _Json_addField = F3(function(key, value, object) {
        object[key] = _Json_unwrap(value);
        return object;
    });
    function _Json_addEntry(func) {
        return F2(function(entry, array) {
            array.push(_Json_unwrap(func(entry)));
            return array;
        });
    }
    var _Json_encodeNull = _Json_wrap(null);
    // TASKS
    function _Scheduler_succeed(value) {
        return {
            $: 0,
            a: value
        };
    }
    function _Scheduler_fail(error) {
        return {
            $: 1,
            a: error
        };
    }
    function _Scheduler_binding(callback) {
        return {
            $: 2,
            b: callback,
            c: null
        };
    }
    var _Scheduler_andThen = F2(function(callback, task) {
        return {
            $: 3,
            b: callback,
            d: task
        };
    });
    var _Scheduler_onError = F2(function(callback, task) {
        return {
            $: 4,
            b: callback,
            d: task
        };
    });
    function _Scheduler_receive(callback) {
        return {
            $: 5,
            b: callback
        };
    }
    // PROCESSES
    var _Scheduler_guid = 0;
    function _Scheduler_rawSpawn(task) {
        var proc = {
            $: 0,
            e: _Scheduler_guid++,
            f: task,
            g: null,
            h: []
        };
        _Scheduler_enqueue(proc);
        return proc;
    }
    function _Scheduler_spawn(task) {
        return _Scheduler_binding(function(callback) {
            callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
        });
    }
    function _Scheduler_rawSend(proc, msg) {
        proc.h.push(msg);
        _Scheduler_enqueue(proc);
    }
    var _Scheduler_send = F2(function(proc, msg) {
        return _Scheduler_binding(function(callback) {
            _Scheduler_rawSend(proc, msg);
            callback(_Scheduler_succeed(_Utils_Tuple0));
        });
    });
    function _Scheduler_kill(proc) {
        return _Scheduler_binding(function(callback) {
            var task = proc.f;
            if (task.$ === 2 && task.c) task.c();
            proc.f = null;
            callback(_Scheduler_succeed(_Utils_Tuple0));
        });
    }
    /* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/ var _Scheduler_working = false;
    var _Scheduler_queue = [];
    function _Scheduler_enqueue(proc) {
        _Scheduler_queue.push(proc);
        if (_Scheduler_working) return;
        _Scheduler_working = true;
        while(proc = _Scheduler_queue.shift())_Scheduler_step(proc);
        _Scheduler_working = false;
    }
    function _Scheduler_step(proc) {
        while(proc.f){
            var rootTag = proc.f.$;
            if (rootTag === 0 || rootTag === 1) {
                while(proc.g && proc.g.$ !== rootTag)proc.g = proc.g.i;
                if (!proc.g) return;
                proc.f = proc.g.b(proc.f.a);
                proc.g = proc.g.i;
            } else if (rootTag === 2) {
                proc.f.c = proc.f.b(function(newRoot) {
                    proc.f = newRoot;
                    _Scheduler_enqueue(proc);
                });
                return;
            } else if (rootTag === 5) {
                if (proc.h.length === 0) return;
                proc.f = proc.f.b(proc.h.shift());
            } else {
                proc.g = {
                    $: rootTag === 3 ? 0 : 1,
                    b: proc.f.b,
                    i: proc.g
                };
                proc.f = proc.f.d;
            }
        }
    }
    function _Process_sleep(time) {
        return _Scheduler_binding(function(callback) {
            var id = setTimeout(function() {
                callback(_Scheduler_succeed(_Utils_Tuple0));
            }, time);
            return function() {
                clearTimeout(id);
            };
        });
    }
    // PROGRAMS
    var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args) {
        return _Platform_initialize(flagDecoder, args, impl.init, impl.update, impl.subscriptions, function() {
            return function() {};
        });
    });
    // INITIALIZE A PROGRAM
    function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder) {
        var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args["flags"] : undefined));
        $elm$core$Result$isOk(result) || _Debug_crash(2 /**/ , _Json_errorToString(result.a));
        var managers = {};
        result = init(result.a);
        var model = result.a;
        var stepper = stepperBuilder(sendToApp, model);
        var ports = _Platform_setupEffects(managers, sendToApp);
        function sendToApp(msg, viewMetadata) {
            result = A2(update, msg, model);
            stepper(model = result.a, viewMetadata);
            _Platform_dispatchEffects(managers, result.b, subscriptions(model));
        }
        _Platform_dispatchEffects(managers, result.b, subscriptions(model));
        return ports ? {
            ports: ports
        } : {};
    }
    // TRACK PRELOADS
    //
    // This is used by code in elm/browser and elm/http
    // to register any HTTP requests that are triggered by init.
    //
    var _Platform_preload;
    function _Platform_registerPreload(url) {
        _Platform_preload.add(url);
    }
    // EFFECT MANAGERS
    var _Platform_effectManagers = {};
    function _Platform_setupEffects(managers, sendToApp) {
        var ports;
        // setup all necessary effect managers
        for(var key in _Platform_effectManagers){
            var manager = _Platform_effectManagers[key];
            if (manager.a) {
                ports = ports || {};
                ports[key] = manager.a(key, sendToApp);
            }
            managers[key] = _Platform_instantiateManager(manager, sendToApp);
        }
        return ports;
    }
    function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap) {
        return {
            b: init,
            c: onEffects,
            d: onSelfMsg,
            e: cmdMap,
            f: subMap
        };
    }
    function _Platform_instantiateManager(info, sendToApp) {
        var router = {
            g: sendToApp,
            h: undefined
        };
        var onEffects = info.c;
        var onSelfMsg = info.d;
        var cmdMap = info.e;
        var subMap = info.f;
        function loop(state) {
            return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg) {
                var value = msg.a;
                if (msg.$ === 0) return A3(onSelfMsg, router, value, state);
                return cmdMap && subMap ? A4(onEffects, router, value.i, value.j, state) : A3(onEffects, router, cmdMap ? value.i : value.j, state);
            }));
        }
        return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
    }
    // ROUTING
    var _Platform_sendToApp = F2(function(router, msg) {
        return _Scheduler_binding(function(callback) {
            router.g(msg);
            callback(_Scheduler_succeed(_Utils_Tuple0));
        });
    });
    var _Platform_sendToSelf = F2(function(router, msg) {
        return A2(_Scheduler_send, router.h, {
            $: 0,
            a: msg
        });
    });
    // BAGS
    function _Platform_leaf(home) {
        return function(value) {
            return {
                $: 1,
                k: home,
                l: value
            };
        };
    }
    function _Platform_batch(list) {
        return {
            $: 2,
            m: list
        };
    }
    var _Platform_map = F2(function(tagger, bag) {
        return {
            $: 3,
            n: tagger,
            o: bag
        };
    });
    // PIPE BAGS INTO EFFECT MANAGERS
    function _Platform_dispatchEffects(managers, cmdBag, subBag) {
        var effectsDict = {};
        _Platform_gatherEffects(true, cmdBag, effectsDict, null);
        _Platform_gatherEffects(false, subBag, effectsDict, null);
        for(var home in managers)_Scheduler_rawSend(managers[home], {
            $: "fx",
            a: effectsDict[home] || {
                i: _List_Nil,
                j: _List_Nil
            }
        });
    }
    function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers) {
        switch(bag.$){
            case 1:
                var home = bag.k;
                var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
                effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
                return;
            case 2:
                for(var list = bag.m; list.b; list = list.b)_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
                return;
            case 3:
                _Platform_gatherEffects(isCmd, bag.o, effectsDict, {
                    p: bag.n,
                    q: taggers
                });
                return;
        }
    }
    function _Platform_toEffect(isCmd, home, taggers, value) {
        function applyTaggers(x) {
            for(var temp = taggers; temp; temp = temp.q)x = temp.p(x);
            return x;
        }
        var map = isCmd ? _Platform_effectManagers[home].e : _Platform_effectManagers[home].f;
        return A2(map, applyTaggers, value);
    }
    function _Platform_insert(isCmd, newEffect, effects) {
        effects = effects || {
            i: _List_Nil,
            j: _List_Nil
        };
        isCmd ? effects.i = _List_Cons(newEffect, effects.i) : effects.j = _List_Cons(newEffect, effects.j);
        return effects;
    }
    // PORTS
    function _Platform_checkPortName(name) {
        if (_Platform_effectManagers[name]) _Debug_crash(3, name);
    }
    // OUTGOING PORTS
    function _Platform_outgoingPort(name, converter) {
        _Platform_checkPortName(name);
        _Platform_effectManagers[name] = {
            e: _Platform_outgoingPortMap,
            r: converter,
            a: _Platform_setupOutgoingPort
        };
        return _Platform_leaf(name);
    }
    var _Platform_outgoingPortMap = F2(function(tagger, value) {
        return value;
    });
    function _Platform_setupOutgoingPort(name) {
        var subs = [];
        var converter = _Platform_effectManagers[name].r;
        // CREATE MANAGER
        var init = _Process_sleep(0);
        _Platform_effectManagers[name].b = init;
        _Platform_effectManagers[name].c = F3(function(router, cmdList, state) {
            for(; cmdList.b; cmdList = cmdList.b){
                // grab a separate reference to subs in case unsubscribe is called
                var currentSubs = subs;
                var value = _Json_unwrap(converter(cmdList.a));
                for(var i = 0; i < currentSubs.length; i++)currentSubs[i](value);
            }
            return init;
        });
        // PUBLIC API
        function subscribe(callback) {
            subs.push(callback);
        }
        function unsubscribe(callback) {
            // copy subs into a new array in case unsubscribe is called within a
            // subscribed callback
            subs = subs.slice();
            var index = subs.indexOf(callback);
            if (index >= 0) subs.splice(index, 1);
        }
        return {
            subscribe: subscribe,
            unsubscribe: unsubscribe
        };
    }
    // INCOMING PORTS
    function _Platform_incomingPort(name, converter) {
        _Platform_checkPortName(name);
        _Platform_effectManagers[name] = {
            f: _Platform_incomingPortMap,
            r: converter,
            a: _Platform_setupIncomingPort
        };
        return _Platform_leaf(name);
    }
    var _Platform_incomingPortMap = F2(function(tagger, finalTagger) {
        return function(value) {
            return tagger(finalTagger(value));
        };
    });
    function _Platform_setupIncomingPort(name, sendToApp) {
        var subs = _List_Nil;
        var converter = _Platform_effectManagers[name].r;
        // CREATE MANAGER
        var init = _Scheduler_succeed(null);
        _Platform_effectManagers[name].b = init;
        _Platform_effectManagers[name].c = F3(function(router, subList, state) {
            subs = subList;
            return init;
        });
        // PUBLIC API
        function send(incomingValue) {
            var result = A2(_Json_run, converter, _Json_wrap(incomingValue));
            $elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);
            var value = result.a;
            for(var temp = subs; temp.b; temp = temp.b)sendToApp(temp.a(value));
        }
        return {
            send: send
        };
    }
    // EXPORT ELM MODULES
    //
    // Have DEBUG and PROD versions so that we can (1) give nicer errors in
    // debug mode and (2) not pay for the bits needed for that in prod mode.
    //
    function _Platform_export_UNUSED(exports) {
        scope["Elm"] ? _Platform_mergeExportsProd(scope["Elm"], exports) : scope["Elm"] = exports;
    }
    function _Platform_mergeExportsProd(obj, exports) {
        for(var name in exports)name in obj ? name == "init" ? _Debug_crash(6) : _Platform_mergeExportsProd(obj[name], exports[name]) : obj[name] = exports[name];
    }
    function _Platform_export(exports) {
        scope["Elm"] ? _Platform_mergeExportsDebug("Elm", scope["Elm"], exports) : scope["Elm"] = exports;
    }
    function _Platform_mergeExportsDebug(moduleName, obj, exports) {
        for(var name in exports)name in obj ? name == "init" ? _Debug_crash(6, moduleName) : _Platform_mergeExportsDebug(moduleName + "." + name, obj[name], exports[name]) : obj[name] = exports[name];
    }
    // HELPERS
    var _VirtualDom_divertHrefToApp;
    var _VirtualDom_doc = typeof document !== "undefined" ? document : {};
    function _VirtualDom_appendChild(parent, child) {
        parent.appendChild(child);
    }
    var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args) {
        // NOTE: this function needs _Platform_export available to work
        /**_UNUSED/
	var node = args['node'];
	//*/ /**/ var node = args && args["node"] ? args["node"] : _Debug_crash(0);
        //*/
        node.parentNode.replaceChild(_VirtualDom_render(virtualNode, function() {}), node);
        return {};
    });
    // TEXT
    function _VirtualDom_text(string) {
        return {
            $: 0,
            a: string
        };
    }
    // NODE
    var _VirtualDom_nodeNS = F2(function(namespace, tag) {
        return F2(function(factList, kidList) {
            for(var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b){
                var kid = kidList.a;
                descendantsCount += kid.b || 0;
                kids.push(kid);
            }
            descendantsCount += kids.length;
            return {
                $: 1,
                c: tag,
                d: _VirtualDom_organizeFacts(factList),
                e: kids,
                f: namespace,
                b: descendantsCount
            };
        });
    });
    var _VirtualDom_node = _VirtualDom_nodeNS(undefined);
    // KEYED NODE
    var _VirtualDom_keyedNodeNS = F2(function(namespace, tag) {
        return F2(function(factList, kidList) {
            for(var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b){
                var kid = kidList.a;
                descendantsCount += kid.b.b || 0;
                kids.push(kid);
            }
            descendantsCount += kids.length;
            return {
                $: 2,
                c: tag,
                d: _VirtualDom_organizeFacts(factList),
                e: kids,
                f: namespace,
                b: descendantsCount
            };
        });
    });
    var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);
    // CUSTOM
    function _VirtualDom_custom(factList, model, render, diff) {
        return {
            $: 3,
            d: _VirtualDom_organizeFacts(factList),
            g: model,
            h: render,
            i: diff
        };
    }
    // MAP
    var _VirtualDom_map = F2(function(tagger, node) {
        return {
            $: 4,
            j: tagger,
            k: node,
            b: 1 + (node.b || 0)
        };
    });
    // LAZY
    function _VirtualDom_thunk(refs, thunk) {
        return {
            $: 5,
            l: refs,
            m: thunk,
            k: undefined
        };
    }
    var _VirtualDom_lazy = F2(function(func, a) {
        return _VirtualDom_thunk([
            func,
            a
        ], function() {
            return func(a);
        });
    });
    var _VirtualDom_lazy2 = F3(function(func, a, b) {
        return _VirtualDom_thunk([
            func,
            a,
            b
        ], function() {
            return A2(func, a, b);
        });
    });
    var _VirtualDom_lazy3 = F4(function(func, a, b, c) {
        return _VirtualDom_thunk([
            func,
            a,
            b,
            c
        ], function() {
            return A3(func, a, b, c);
        });
    });
    var _VirtualDom_lazy4 = F5(function(func, a, b, c, d) {
        return _VirtualDom_thunk([
            func,
            a,
            b,
            c,
            d
        ], function() {
            return A4(func, a, b, c, d);
        });
    });
    var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e) {
        return _VirtualDom_thunk([
            func,
            a,
            b,
            c,
            d,
            e
        ], function() {
            return A5(func, a, b, c, d, e);
        });
    });
    var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f) {
        return _VirtualDom_thunk([
            func,
            a,
            b,
            c,
            d,
            e,
            f
        ], function() {
            return A6(func, a, b, c, d, e, f);
        });
    });
    var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g) {
        return _VirtualDom_thunk([
            func,
            a,
            b,
            c,
            d,
            e,
            f,
            g
        ], function() {
            return A7(func, a, b, c, d, e, f, g);
        });
    });
    var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h) {
        return _VirtualDom_thunk([
            func,
            a,
            b,
            c,
            d,
            e,
            f,
            g,
            h
        ], function() {
            return A8(func, a, b, c, d, e, f, g, h);
        });
    });
    // FACTS
    var _VirtualDom_on = F2(function(key, handler) {
        return {
            $: "a0",
            n: key,
            o: handler
        };
    });
    var _VirtualDom_style = F2(function(key, value) {
        return {
            $: "a1",
            n: key,
            o: value
        };
    });
    var _VirtualDom_property = F2(function(key, value) {
        return {
            $: "a2",
            n: key,
            o: value
        };
    });
    var _VirtualDom_attribute = F2(function(key, value) {
        return {
            $: "a3",
            n: key,
            o: value
        };
    });
    var _VirtualDom_attributeNS = F3(function(namespace, key, value) {
        return {
            $: "a4",
            n: key,
            o: {
                f: namespace,
                o: value
            }
        };
    });
    // XSS ATTACK VECTOR CHECKS
    function _VirtualDom_noScript(tag) {
        return tag == "script" ? "p" : tag;
    }
    function _VirtualDom_noOnOrFormAction(key) {
        return /^(on|formAction$)/i.test(key) ? "data-" + key : key;
    }
    function _VirtualDom_noInnerHtmlOrFormAction(key) {
        return key == "innerHTML" || key == "formAction" ? "data-" + key : key;
    }
    function _VirtualDom_noJavaScriptUri_UNUSED(value) {
        return /^javascript:/i.test(value.replace(/\s/g, "")) ? "" : value;
    }
    function _VirtualDom_noJavaScriptUri(value) {
        return /^javascript:/i.test(value.replace(/\s/g, "")) ? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")' : value;
    }
    function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value) {
        return /^\s*(javascript:|data:text\/html)/i.test(value) ? "" : value;
    }
    function _VirtualDom_noJavaScriptOrHtmlUri(value) {
        return /^\s*(javascript:|data:text\/html)/i.test(value) ? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")' : value;
    }
    // MAP FACTS
    var _VirtualDom_mapAttribute = F2(function(func, attr) {
        return attr.$ === "a0" ? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o)) : attr;
    });
    function _VirtualDom_mapHandler(func, handler) {
        var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);
        // 0 = Normal
        // 1 = MayStopPropagation
        // 2 = MayPreventDefault
        // 3 = Custom
        return {
            $: handler.$,
            a: !tag ? A2($elm$json$Json$Decode$map, func, handler.a) : A3($elm$json$Json$Decode$map2, tag < 3 ? _VirtualDom_mapEventTuple : _VirtualDom_mapEventRecord, $elm$json$Json$Decode$succeed(func), handler.a)
        };
    }
    var _VirtualDom_mapEventTuple = F2(function(func, tuple) {
        return _Utils_Tuple2(func(tuple.a), tuple.b);
    });
    var _VirtualDom_mapEventRecord = F2(function(func, record) {
        return {
            message: func(record.message),
            stopPropagation: record.stopPropagation,
            preventDefault: record.preventDefault
        };
    });
    // ORGANIZE FACTS
    function _VirtualDom_organizeFacts(factList) {
        for(var facts = {}; factList.b; factList = factList.b){
            var entry = factList.a;
            var tag = entry.$;
            var key = entry.n;
            var value = entry.o;
            if (tag === "a2") {
                key === "className" ? _VirtualDom_addClass(facts, key, _Json_unwrap(value)) : facts[key] = _Json_unwrap(value);
                continue;
            }
            var subFacts = facts[tag] || (facts[tag] = {});
            tag === "a3" && key === "class" ? _VirtualDom_addClass(subFacts, key, value) : subFacts[key] = value;
        }
        return facts;
    }
    function _VirtualDom_addClass(object, key, newClass) {
        var classes = object[key];
        object[key] = classes ? classes + " " + newClass : newClass;
    }
    // RENDER
    function _VirtualDom_render(vNode, eventNode) {
        var tag = vNode.$;
        if (tag === 5) return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
        if (tag === 0) return _VirtualDom_doc.createTextNode(vNode.a);
        if (tag === 4) {
            var subNode = vNode.k;
            var tagger = vNode.j;
            while(subNode.$ === 4){
                typeof tagger !== "object" ? tagger = [
                    tagger,
                    subNode.j
                ] : tagger.push(subNode.j);
                subNode = subNode.k;
            }
            var subEventRoot = {
                j: tagger,
                p: eventNode
            };
            var domNode = _VirtualDom_render(subNode, subEventRoot);
            domNode.elm_event_node_ref = subEventRoot;
            return domNode;
        }
        if (tag === 3) {
            var domNode = vNode.h(vNode.g);
            _VirtualDom_applyFacts(domNode, eventNode, vNode.d);
            return domNode;
        }
        // at this point `tag` must be 1 or 2
        var domNode = vNode.f ? _VirtualDom_doc.createElementNS(vNode.f, vNode.c) : _VirtualDom_doc.createElement(vNode.c);
        if (_VirtualDom_divertHrefToApp && vNode.c == "a") domNode.addEventListener("click", _VirtualDom_divertHrefToApp(domNode));
        _VirtualDom_applyFacts(domNode, eventNode, vNode.d);
        for(var kids = vNode.e, i = 0; i < kids.length; i++)_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
        return domNode;
    }
    // APPLY FACTS
    function _VirtualDom_applyFacts(domNode, eventNode, facts) {
        for(var key in facts){
            var value = facts[key];
            key === "a1" ? _VirtualDom_applyStyles(domNode, value) : key === "a0" ? _VirtualDom_applyEvents(domNode, eventNode, value) : key === "a3" ? _VirtualDom_applyAttrs(domNode, value) : key === "a4" ? _VirtualDom_applyAttrsNS(domNode, value) : (key !== "value" && key !== "checked" || domNode[key] !== value) && (domNode[key] = value);
        }
    }
    // APPLY STYLES
    function _VirtualDom_applyStyles(domNode, styles) {
        var domNodeStyle = domNode.style;
        for(var key in styles)domNodeStyle[key] = styles[key];
    }
    // APPLY ATTRS
    function _VirtualDom_applyAttrs(domNode, attrs) {
        for(var key in attrs){
            var value = attrs[key];
            typeof value !== "undefined" ? domNode.setAttribute(key, value) : domNode.removeAttribute(key);
        }
    }
    // APPLY NAMESPACED ATTRS
    function _VirtualDom_applyAttrsNS(domNode, nsAttrs) {
        for(var key in nsAttrs){
            var pair = nsAttrs[key];
            var namespace = pair.f;
            var value = pair.o;
            typeof value !== "undefined" ? domNode.setAttributeNS(namespace, key, value) : domNode.removeAttributeNS(namespace, key);
        }
    }
    // APPLY EVENTS
    function _VirtualDom_applyEvents(domNode, eventNode, events) {
        var allCallbacks = domNode.elmFs || (domNode.elmFs = {});
        for(var key in events){
            var newHandler = events[key];
            var oldCallback = allCallbacks[key];
            if (!newHandler) {
                domNode.removeEventListener(key, oldCallback);
                allCallbacks[key] = undefined;
                continue;
            }
            if (oldCallback) {
                var oldHandler = oldCallback.q;
                if (oldHandler.$ === newHandler.$) {
                    oldCallback.q = newHandler;
                    continue;
                }
                domNode.removeEventListener(key, oldCallback);
            }
            oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
            domNode.addEventListener(key, oldCallback, _VirtualDom_passiveSupported && {
                passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2
            });
            allCallbacks[key] = oldCallback;
        }
    }
    // PASSIVE EVENTS
    var _VirtualDom_passiveSupported;
    try {
        window.addEventListener("t", null, Object.defineProperty({}, "passive", {
            get: function() {
                _VirtualDom_passiveSupported = true;
            }
        }));
    } catch (e) {}
    // EVENT HANDLERS
    function _VirtualDom_makeCallback(eventNode, initialHandler) {
        function callback(event) {
            var handler = callback.q;
            var result = _Json_runHelp(handler.a, event);
            if (!$elm$core$Result$isOk(result)) return;
            var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);
            // 0 = Normal
            // 1 = MayStopPropagation
            // 2 = MayPreventDefault
            // 3 = Custom
            var value = result.a;
            var message = !tag ? value : tag < 3 ? value.a : value.message;
            var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
            var currentEventNode = (stopPropagation && event.stopPropagation(), (tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(), eventNode);
            var tagger;
            var i;
            while(tagger = currentEventNode.j){
                if (typeof tagger == "function") message = tagger(message);
                else for(var i = tagger.length; i--;)message = tagger[i](message);
                currentEventNode = currentEventNode.p;
            }
            currentEventNode(message, stopPropagation); // stopPropagation implies isSync
        }
        callback.q = initialHandler;
        return callback;
    }
    function _VirtualDom_equalEvents(x, y) {
        return x.$ == y.$ && _Json_equality(x.a, y.a);
    }
    // DIFF
    // TODO: Should we do patches like in iOS?
    //
    // type Patch
    //   = At Int Patch
    //   | Batch (List Patch)
    //   | Change ...
    //
    // How could it not be better?
    //
    function _VirtualDom_diff(x, y) {
        var patches = [];
        _VirtualDom_diffHelp(x, y, patches, 0);
        return patches;
    }
    function _VirtualDom_pushPatch(patches, type, index, data) {
        var patch = {
            $: type,
            r: index,
            s: data,
            t: undefined,
            u: undefined
        };
        patches.push(patch);
        return patch;
    }
    function _VirtualDom_diffHelp(x, y, patches, index) {
        if (x === y) return;
        var xType = x.$;
        var yType = y.$;
        // Bail if you run into different types of nodes. Implies that the
        // structure has changed significantly and it's not worth a diff.
        if (xType !== yType) {
            if (xType === 1 && yType === 2) {
                y = _VirtualDom_dekey(y);
                yType = 1;
            } else {
                _VirtualDom_pushPatch(patches, 0, index, y);
                return;
            }
        }
        // Now we know that both nodes are the same $.
        switch(yType){
            case 5:
                var xRefs = x.l;
                var yRefs = y.l;
                var i = xRefs.length;
                var same = i === yRefs.length;
                while(same && i--)same = xRefs[i] === yRefs[i];
                if (same) {
                    y.k = x.k;
                    return;
                }
                y.k = y.m();
                var subPatches = [];
                _VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
                subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
                return;
            case 4:
                // gather nested taggers
                var xTaggers = x.j;
                var yTaggers = y.j;
                var nesting = false;
                var xSubNode = x.k;
                while(xSubNode.$ === 4){
                    nesting = true;
                    typeof xTaggers !== "object" ? xTaggers = [
                        xTaggers,
                        xSubNode.j
                    ] : xTaggers.push(xSubNode.j);
                    xSubNode = xSubNode.k;
                }
                var ySubNode = y.k;
                while(ySubNode.$ === 4){
                    nesting = true;
                    typeof yTaggers !== "object" ? yTaggers = [
                        yTaggers,
                        ySubNode.j
                    ] : yTaggers.push(ySubNode.j);
                    ySubNode = ySubNode.k;
                }
                // Just bail if different numbers of taggers. This implies the
                // structure of the virtual DOM has changed.
                if (nesting && xTaggers.length !== yTaggers.length) {
                    _VirtualDom_pushPatch(patches, 0, index, y);
                    return;
                }
                // check if taggers are "the same"
                if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers) _VirtualDom_pushPatch(patches, 2, index, yTaggers);
                // diff everything below the taggers
                _VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
                return;
            case 0:
                if (x.a !== y.a) _VirtualDom_pushPatch(patches, 3, index, y.a);
                return;
            case 1:
                _VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
                return;
            case 2:
                _VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
                return;
            case 3:
                if (x.h !== y.h) {
                    _VirtualDom_pushPatch(patches, 0, index, y);
                    return;
                }
                var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
                factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);
                var patch = y.i(x.g, y.g);
                patch && _VirtualDom_pushPatch(patches, 5, index, patch);
                return;
        }
    }
    // assumes the incoming arrays are the same length
    function _VirtualDom_pairwiseRefEqual(as, bs) {
        for(var i = 0; i < as.length; i++){
            if (as[i] !== bs[i]) return false;
        }
        return true;
    }
    function _VirtualDom_diffNodes(x, y, patches, index, diffKids) {
        // Bail if obvious indicators have changed. Implies more serious
        // structural changes such that it's not worth it to diff.
        if (x.c !== y.c || x.f !== y.f) {
            _VirtualDom_pushPatch(patches, 0, index, y);
            return;
        }
        var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
        factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);
        diffKids(x, y, patches, index);
    }
    // DIFF FACTS
    // TODO Instead of creating a new diff object, it's possible to just test if
    // there *is* a diff. During the actual patch, do the diff again and make the
    // modifications directly. This way, there's no new allocations. Worth it?
    function _VirtualDom_diffFacts(x, y, category) {
        var diff;
        // look for changes and removals
        for(var xKey in x){
            if (xKey === "a1" || xKey === "a0" || xKey === "a3" || xKey === "a4") {
                var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
                if (subDiff) {
                    diff = diff || {};
                    diff[xKey] = subDiff;
                }
                continue;
            }
            // remove if not in the new facts
            if (!(xKey in y)) {
                diff = diff || {};
                diff[xKey] = !category ? typeof x[xKey] === "string" ? "" : null : category === "a1" ? "" : category === "a0" || category === "a3" ? undefined : {
                    f: x[xKey].f,
                    o: undefined
                };
                continue;
            }
            var xValue = x[xKey];
            var yValue = y[xKey];
            // reference equal, so don't worry about it
            if (xValue === yValue && xKey !== "value" && xKey !== "checked" || category === "a0" && _VirtualDom_equalEvents(xValue, yValue)) continue;
            diff = diff || {};
            diff[xKey] = yValue;
        }
        // add new stuff
        for(var yKey in y)if (!(yKey in x)) {
            diff = diff || {};
            diff[yKey] = y[yKey];
        }
        return diff;
    }
    // DIFF KIDS
    function _VirtualDom_diffKids(xParent, yParent, patches, index) {
        var xKids = xParent.e;
        var yKids = yParent.e;
        var xLen = xKids.length;
        var yLen = yKids.length;
        // FIGURE OUT IF THERE ARE INSERTS OR REMOVALS
        if (xLen > yLen) _VirtualDom_pushPatch(patches, 6, index, {
            v: yLen,
            i: xLen - yLen
        });
        else if (xLen < yLen) _VirtualDom_pushPatch(patches, 7, index, {
            v: xLen,
            e: yKids
        });
        // PAIRWISE DIFF EVERYTHING ELSE
        for(var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++){
            var xKid = xKids[i];
            _VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
            index += xKid.b || 0;
        }
    }
    // KEYED DIFF
    function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex) {
        var localPatches = [];
        var changes = {}; // Dict String Entry
        var inserts = []; // Array { index : Int, entry : Entry }
        // type Entry = { tag : String, vnode : VNode, index : Int, data : _ }
        var xKids = xParent.e;
        var yKids = yParent.e;
        var xLen = xKids.length;
        var yLen = yKids.length;
        var xIndex = 0;
        var yIndex = 0;
        var index = rootIndex;
        while(xIndex < xLen && yIndex < yLen){
            var x = xKids[xIndex];
            var y = yKids[yIndex];
            var xKey = x.a;
            var yKey = y.a;
            var xNode = x.b;
            var yNode = y.b;
            var newMatch = undefined;
            var oldMatch = undefined;
            // check if keys match
            if (xKey === yKey) {
                index++;
                _VirtualDom_diffHelp(xNode, yNode, localPatches, index);
                index += xNode.b || 0;
                xIndex++;
                yIndex++;
                continue;
            }
            // look ahead 1 to detect insertions and removals.
            var xNext = xKids[xIndex + 1];
            var yNext = yKids[yIndex + 1];
            if (xNext) {
                var xNextKey = xNext.a;
                var xNextNode = xNext.b;
                oldMatch = yKey === xNextKey;
            }
            if (yNext) {
                var yNextKey = yNext.a;
                var yNextNode = yNext.b;
                newMatch = xKey === yNextKey;
            }
            // swap x and y
            if (newMatch && oldMatch) {
                index++;
                _VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
                _VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
                index += xNode.b || 0;
                index++;
                _VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
                index += xNextNode.b || 0;
                xIndex += 2;
                yIndex += 2;
                continue;
            }
            // insert y
            if (newMatch) {
                index++;
                _VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
                _VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
                index += xNode.b || 0;
                xIndex += 1;
                yIndex += 2;
                continue;
            }
            // remove x
            if (oldMatch) {
                index++;
                _VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
                index += xNode.b || 0;
                index++;
                _VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
                index += xNextNode.b || 0;
                xIndex += 2;
                yIndex += 1;
                continue;
            }
            // remove x, insert y
            if (xNext && xNextKey === yNextKey) {
                index++;
                _VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
                _VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
                index += xNode.b || 0;
                index++;
                _VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
                index += xNextNode.b || 0;
                xIndex += 2;
                yIndex += 2;
                continue;
            }
            break;
        }
        // eat up any remaining nodes with removeNode and insertNode
        while(xIndex < xLen){
            index++;
            var x = xKids[xIndex];
            var xNode = x.b;
            _VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
            index += xNode.b || 0;
            xIndex++;
        }
        while(yIndex < yLen){
            var endInserts = endInserts || [];
            var y = yKids[yIndex];
            _VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
            yIndex++;
        }
        if (localPatches.length > 0 || inserts.length > 0 || endInserts) _VirtualDom_pushPatch(patches, 8, rootIndex, {
            w: localPatches,
            x: inserts,
            y: endInserts
        });
    }
    // CHANGES FROM KEYED DIFF
    var _VirtualDom_POSTFIX = "_elmW6BL";
    function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts) {
        var entry = changes[key];
        // never seen this key before
        if (!entry) {
            entry = {
                c: 0,
                z: vnode,
                r: yIndex,
                s: undefined
            };
            inserts.push({
                r: yIndex,
                A: entry
            });
            changes[key] = entry;
            return;
        }
        // this key was removed earlier, a match!
        if (entry.c === 1) {
            inserts.push({
                r: yIndex,
                A: entry
            });
            entry.c = 2;
            var subPatches = [];
            _VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
            entry.r = yIndex;
            entry.s.s = {
                w: subPatches,
                A: entry
            };
            return;
        }
        // this key has already been inserted or moved, a duplicate!
        _VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
    }
    function _VirtualDom_removeNode(changes, localPatches, key, vnode, index) {
        var entry = changes[key];
        // never seen this key before
        if (!entry) {
            var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);
            changes[key] = {
                c: 1,
                z: vnode,
                r: index,
                s: patch
            };
            return;
        }
        // this key was inserted earlier, a match!
        if (entry.c === 0) {
            entry.c = 2;
            var subPatches = [];
            _VirtualDom_diffHelp(vnode, entry.z, subPatches, index);
            _VirtualDom_pushPatch(localPatches, 9, index, {
                w: subPatches,
                A: entry
            });
            return;
        }
        // this key has already been removed or moved, a duplicate!
        _VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
    }
    // ADD DOM NODES
    //
    // Each DOM node has an "index" assigned in order of traversal. It is important
    // to minimize our crawl over the actual DOM, so these indexes (along with the
    // descendantsCount of virtual nodes) let us skip touching entire subtrees of
    // the DOM if we know there are no patches there.
    function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode) {
        _VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
    }
    // assumes `patches` is non-empty and indexes increase monotonically.
    function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode) {
        var patch = patches[i];
        var index = patch.r;
        while(index === low){
            var patchType = patch.$;
            if (patchType === 1) _VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
            else if (patchType === 8) {
                patch.t = domNode;
                patch.u = eventNode;
                var subPatches = patch.s.w;
                if (subPatches.length > 0) _VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
            } else if (patchType === 9) {
                patch.t = domNode;
                patch.u = eventNode;
                var data = patch.s;
                if (data) {
                    data.A.s = domNode;
                    var subPatches = data.w;
                    if (subPatches.length > 0) _VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
                }
            } else {
                patch.t = domNode;
                patch.u = eventNode;
            }
            i++;
            if (!(patch = patches[i]) || (index = patch.r) > high) return i;
        }
        var tag = vNode.$;
        if (tag === 4) {
            var subNode = vNode.k;
            while(subNode.$ === 4)subNode = subNode.k;
            return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
        }
        // tag must be 1 or 2 at this point
        var vKids = vNode.e;
        var childNodes = domNode.childNodes;
        for(var j = 0; j < vKids.length; j++){
            low++;
            var vKid = tag === 1 ? vKids[j] : vKids[j].b;
            var nextLow = low + (vKid.b || 0);
            if (low <= index && index <= nextLow) {
                i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
                if (!(patch = patches[i]) || (index = patch.r) > high) return i;
            }
            low = nextLow;
        }
        return i;
    }
    // APPLY PATCHES
    function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode) {
        if (patches.length === 0) return rootDomNode;
        _VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
        return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
    }
    function _VirtualDom_applyPatchesHelp(rootDomNode, patches) {
        for(var i = 0; i < patches.length; i++){
            var patch = patches[i];
            var localDomNode = patch.t;
            var newNode = _VirtualDom_applyPatch(localDomNode, patch);
            if (localDomNode === rootDomNode) rootDomNode = newNode;
        }
        return rootDomNode;
    }
    function _VirtualDom_applyPatch(domNode, patch) {
        switch(patch.$){
            case 0:
                return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);
            case 4:
                _VirtualDom_applyFacts(domNode, patch.u, patch.s);
                return domNode;
            case 3:
                domNode.replaceData(0, domNode.length, patch.s);
                return domNode;
            case 1:
                return _VirtualDom_applyPatchesHelp(domNode, patch.s);
            case 2:
                if (domNode.elm_event_node_ref) domNode.elm_event_node_ref.j = patch.s;
                else domNode.elm_event_node_ref = {
                    j: patch.s,
                    p: patch.u
                };
                return domNode;
            case 6:
                var data = patch.s;
                for(var i = 0; i < data.i; i++)domNode.removeChild(domNode.childNodes[data.v]);
                return domNode;
            case 7:
                var data = patch.s;
                var kids = data.e;
                var i = data.v;
                var theEnd = domNode.childNodes[i];
                for(; i < kids.length; i++)domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
                return domNode;
            case 9:
                var data = patch.s;
                if (!data) {
                    domNode.parentNode.removeChild(domNode);
                    return domNode;
                }
                var entry = data.A;
                if (typeof entry.r !== "undefined") domNode.parentNode.removeChild(domNode);
                entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
                return domNode;
            case 8:
                return _VirtualDom_applyPatchReorder(domNode, patch);
            case 5:
                return patch.s(domNode);
            default:
                _Debug_crash(10); // 'Ran into an unknown patch!'
        }
    }
    function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode) {
        var parentNode = domNode.parentNode;
        var newNode = _VirtualDom_render(vNode, eventNode);
        if (!newNode.elm_event_node_ref) newNode.elm_event_node_ref = domNode.elm_event_node_ref;
        if (parentNode && newNode !== domNode) parentNode.replaceChild(newNode, domNode);
        return newNode;
    }
    function _VirtualDom_applyPatchReorder(domNode, patch) {
        var data = patch.s;
        // remove end inserts
        var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);
        // removals
        domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);
        // inserts
        var inserts = data.x;
        for(var i = 0; i < inserts.length; i++){
            var insert = inserts[i];
            var entry = insert.A;
            var node = entry.c === 2 ? entry.s : _VirtualDom_render(entry.z, patch.u);
            domNode.insertBefore(node, domNode.childNodes[insert.r]);
        }
        // add end inserts
        if (frag) _VirtualDom_appendChild(domNode, frag);
        return domNode;
    }
    function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch) {
        if (!endInserts) return;
        var frag = _VirtualDom_doc.createDocumentFragment();
        for(var i = 0; i < endInserts.length; i++){
            var insert = endInserts[i];
            var entry = insert.A;
            _VirtualDom_appendChild(frag, entry.c === 2 ? entry.s : _VirtualDom_render(entry.z, patch.u));
        }
        return frag;
    }
    function _VirtualDom_virtualize(node) {
        // TEXT NODES
        if (node.nodeType === 3) return _VirtualDom_text(node.textContent);
        // WEIRD NODES
        if (node.nodeType !== 1) return _VirtualDom_text("");
        // ELEMENT NODES
        var attrList = _List_Nil;
        var attrs = node.attributes;
        for(var i = attrs.length; i--;){
            var attr = attrs[i];
            var name = attr.name;
            var value = attr.value;
            attrList = _List_Cons(A2(_VirtualDom_attribute, name, value), attrList);
        }
        var tag = node.tagName.toLowerCase();
        var kidList = _List_Nil;
        var kids = node.childNodes;
        for(var i = kids.length; i--;)kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
        return A3(_VirtualDom_node, tag, attrList, kidList);
    }
    function _VirtualDom_dekey(keyedNode) {
        var keyedKids = keyedNode.e;
        var len = keyedKids.length;
        var kids = new Array(len);
        for(var i = 0; i < len; i++)kids[i] = keyedKids[i].b;
        return {
            $: 1,
            c: keyedNode.c,
            d: keyedNode.d,
            e: kids,
            f: keyedNode.f,
            b: keyedNode.b
        };
    }
    var _Bitwise_and = F2(function(a, b) {
        return a & b;
    });
    var _Bitwise_or = F2(function(a, b) {
        return a | b;
    });
    var _Bitwise_xor = F2(function(a, b) {
        return a ^ b;
    });
    function _Bitwise_complement(a) {
        return ~a;
    }
    var _Bitwise_shiftLeftBy = F2(function(offset, a) {
        return a << offset;
    });
    var _Bitwise_shiftRightBy = F2(function(offset, a) {
        return a >> offset;
    });
    var _Bitwise_shiftRightZfBy = F2(function(offset, a) {
        return a >>> offset;
    });
    // HELPERS
    function _Debugger_unsafeCoerce(value) {
        return value;
    }
    // PROGRAMS
    var _Debugger_element = F4(function(impl, flagDecoder, debugMetadata, args) {
        return _Platform_initialize(flagDecoder, args, A3($elm$browser$Debugger$Main$wrapInit, _Json_wrap(debugMetadata), _Debugger_popout(), impl.init), $elm$browser$Debugger$Main$wrapUpdate(impl.update), $elm$browser$Debugger$Main$wrapSubs(impl.subscriptions), function(sendToApp, initialModel) {
            var view = impl.view;
            var title = _VirtualDom_doc.title;
            var domNode = args && args["node"] ? args["node"] : _Debug_crash(0);
            var currNode = _VirtualDom_virtualize(domNode);
            var currBlocker = $elm$browser$Debugger$Main$toBlockerType(initialModel);
            var currPopout;
            var cornerNode = _VirtualDom_doc.createElement("div");
            domNode.parentNode.insertBefore(cornerNode, domNode.nextSibling);
            var cornerCurr = _VirtualDom_virtualize(cornerNode);
            initialModel.popout.a = sendToApp;
            return _Browser_makeAnimator(initialModel, function(model) {
                var nextNode = A2(_VirtualDom_map, $elm$browser$Debugger$Main$UserMsg, view($elm$browser$Debugger$Main$getUserModel(model)));
                var patches = _VirtualDom_diff(currNode, nextNode);
                domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
                currNode = nextNode;
                // update blocker
                var nextBlocker = $elm$browser$Debugger$Main$toBlockerType(model);
                _Debugger_updateBlocker(currBlocker, nextBlocker);
                currBlocker = nextBlocker;
                // view corner
                var cornerNext = $elm$browser$Debugger$Main$cornerView(model);
                var cornerPatches = _VirtualDom_diff(cornerCurr, cornerNext);
                cornerNode = _VirtualDom_applyPatches(cornerNode, cornerCurr, cornerPatches, sendToApp);
                cornerCurr = cornerNext;
                if (!model.popout.b) {
                    currPopout = undefined;
                    return;
                }
                // view popout
                _VirtualDom_doc = model.popout.b; // SWITCH TO POPOUT DOC
                currPopout || (currPopout = _VirtualDom_virtualize(model.popout.b));
                var nextPopout = $elm$browser$Debugger$Main$popoutView(model);
                var popoutPatches = _VirtualDom_diff(currPopout, nextPopout);
                _VirtualDom_applyPatches(model.popout.b.body, currPopout, popoutPatches, sendToApp);
                currPopout = nextPopout;
                _VirtualDom_doc = document; // SWITCH BACK TO NORMAL DOC
            });
        });
    });
    var _Debugger_document = F4(function(impl, flagDecoder, debugMetadata, args) {
        return _Platform_initialize(flagDecoder, args, A3($elm$browser$Debugger$Main$wrapInit, _Json_wrap(debugMetadata), _Debugger_popout(), impl.init), $elm$browser$Debugger$Main$wrapUpdate(impl.update), $elm$browser$Debugger$Main$wrapSubs(impl.subscriptions), function(sendToApp, initialModel) {
            var divertHrefToApp = impl.setup && impl.setup(function(x) {
                return sendToApp($elm$browser$Debugger$Main$UserMsg(x));
            });
            var view = impl.view;
            var title = _VirtualDom_doc.title;
            var bodyNode = _VirtualDom_doc.body;
            var currNode = _VirtualDom_virtualize(bodyNode);
            var currBlocker = $elm$browser$Debugger$Main$toBlockerType(initialModel);
            var currPopout;
            initialModel.popout.a = sendToApp;
            return _Browser_makeAnimator(initialModel, function(model) {
                _VirtualDom_divertHrefToApp = divertHrefToApp;
                var doc = view($elm$browser$Debugger$Main$getUserModel(model));
                var nextNode = _VirtualDom_node("body")(_List_Nil)(_Utils_ap(A2($elm$core$List$map, _VirtualDom_map($elm$browser$Debugger$Main$UserMsg), doc.body), _List_Cons($elm$browser$Debugger$Main$cornerView(model), _List_Nil)));
                var patches = _VirtualDom_diff(currNode, nextNode);
                bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
                currNode = nextNode;
                _VirtualDom_divertHrefToApp = 0;
                title !== doc.title && (_VirtualDom_doc.title = title = doc.title);
                // update blocker
                var nextBlocker = $elm$browser$Debugger$Main$toBlockerType(model);
                _Debugger_updateBlocker(currBlocker, nextBlocker);
                currBlocker = nextBlocker;
                // view popout
                if (!model.popout.b) {
                    currPopout = undefined;
                    return;
                }
                _VirtualDom_doc = model.popout.b; // SWITCH TO POPOUT DOC
                currPopout || (currPopout = _VirtualDom_virtualize(model.popout.b));
                var nextPopout = $elm$browser$Debugger$Main$popoutView(model);
                var popoutPatches = _VirtualDom_diff(currPopout, nextPopout);
                _VirtualDom_applyPatches(model.popout.b.body, currPopout, popoutPatches, sendToApp);
                currPopout = nextPopout;
                _VirtualDom_doc = document; // SWITCH BACK TO NORMAL DOC
            });
        });
    });
    function _Debugger_popout() {
        return {
            b: undefined,
            a: undefined
        };
    }
    function _Debugger_isOpen(popout) {
        return !!popout.b;
    }
    function _Debugger_open(popout) {
        return _Scheduler_binding(function(callback) {
            _Debugger_openWindow(popout);
            callback(_Scheduler_succeed(_Utils_Tuple0));
        });
    }
    function _Debugger_openWindow(popout) {
        var w = $elm$browser$Debugger$Main$initialWindowWidth, h = $elm$browser$Debugger$Main$initialWindowHeight, x = screen.width - w, y = screen.height - h;
        var debuggerWindow = window.open("", "", "width=" + w + ",height=" + h + ",left=" + x + ",top=" + y);
        var doc = debuggerWindow.document;
        doc.title = "Elm Debugger";
        // handle arrow keys
        doc.addEventListener("keydown", function(event) {
            event.metaKey && event.which === 82 && window.location.reload();
            event.key === "ArrowUp" && (popout.a($elm$browser$Debugger$Main$Up), event.preventDefault());
            event.key === "ArrowDown" && (popout.a($elm$browser$Debugger$Main$Down), event.preventDefault());
        });
        // handle window close
        window.addEventListener("unload", close);
        debuggerWindow.addEventListener("unload", function() {
            popout.b = undefined;
            popout.a($elm$browser$Debugger$Main$NoOp);
            window.removeEventListener("unload", close);
        });
        function close() {
            popout.b = undefined;
            popout.a($elm$browser$Debugger$Main$NoOp);
            debuggerWindow.close();
        }
        // register new window
        popout.b = doc;
    }
    // SCROLL
    function _Debugger_scroll(popout) {
        return _Scheduler_binding(function(callback) {
            if (popout.b) {
                var msgs = popout.b.getElementById("elm-debugger-sidebar");
                if (msgs && msgs.scrollTop !== 0) msgs.scrollTop = 0;
            }
            callback(_Scheduler_succeed(_Utils_Tuple0));
        });
    }
    var _Debugger_scrollTo = F2(function(id, popout) {
        return _Scheduler_binding(function(callback) {
            if (popout.b) {
                var msg = popout.b.getElementById(id);
                if (msg) msg.scrollIntoView(false);
            }
            callback(_Scheduler_succeed(_Utils_Tuple0));
        });
    });
    // UPLOAD
    function _Debugger_upload(popout) {
        return _Scheduler_binding(function(callback) {
            var doc = popout.b || document;
            var element = doc.createElement("input");
            element.setAttribute("type", "file");
            element.setAttribute("accept", "text/json");
            element.style.display = "none";
            element.addEventListener("change", function(event) {
                var fileReader = new FileReader();
                fileReader.onload = function(e) {
                    callback(_Scheduler_succeed(e.target.result));
                };
                fileReader.readAsText(event.target.files[0]);
                doc.body.removeChild(element);
            });
            doc.body.appendChild(element);
            element.click();
        });
    }
    // DOWNLOAD
    var _Debugger_download = F2(function(historyLength, json) {
        return _Scheduler_binding(function(callback) {
            var fileName = "history-" + historyLength + ".txt";
            var jsonString = JSON.stringify(json);
            var mime = "text/plain;charset=utf-8";
            var done = _Scheduler_succeed(_Utils_Tuple0);
            // for IE10+
            if (navigator.msSaveBlob) {
                navigator.msSaveBlob(new Blob([
                    jsonString
                ], {
                    type: mime
                }), fileName);
                return callback(done);
            }
            // for HTML5
            var element = document.createElement("a");
            element.setAttribute("href", "data:" + mime + "," + encodeURIComponent(jsonString));
            element.setAttribute("download", fileName);
            element.style.display = "none";
            document.body.appendChild(element);
            element.click();
            document.body.removeChild(element);
            callback(done);
        });
    });
    // POPOUT CONTENT
    function _Debugger_messageToString(value) {
        if (typeof value === "boolean") return value ? "True" : "False";
        if (typeof value === "number") return value + "";
        if (typeof value === "string") return '"' + _Debugger_addSlashes(value, false) + '"';
        if (value instanceof String) return "'" + _Debugger_addSlashes(value, true) + "'";
        if (typeof value !== "object" || value === null || !("$" in value)) return "…";
        if (typeof value.$ === "number") return "…";
        var code = value.$.charCodeAt(0);
        if (code === 0x23 /* # */  || /* a */ 0x61 <= code && code <= 0x7A /* z */ ) return "…";
        if ([
            "Array_elm_builtin",
            "Set_elm_builtin",
            "RBNode_elm_builtin",
            "RBEmpty_elm_builtin"
        ].indexOf(value.$) >= 0) return "…";
        var keys = Object.keys(value);
        switch(keys.length){
            case 1:
                return value.$;
            case 2:
                return value.$ + " " + _Debugger_messageToString(value.a);
            default:
                return value.$ + " … " + _Debugger_messageToString(value[keys[keys.length - 1]]);
        }
    }
    function _Debugger_init(value) {
        if (typeof value === "boolean") return A3($elm$browser$Debugger$Expando$Constructor, $elm$core$Maybe$Just(value ? "True" : "False"), true, _List_Nil);
        if (typeof value === "number") return $elm$browser$Debugger$Expando$Primitive(value + "");
        if (typeof value === "string") return $elm$browser$Debugger$Expando$S('"' + _Debugger_addSlashes(value, false) + '"');
        if (value instanceof String) return $elm$browser$Debugger$Expando$S("'" + _Debugger_addSlashes(value, true) + "'");
        if (typeof value === "object" && "$" in value) {
            var tag = value.$;
            if (tag === "::" || tag === "[]") return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$ListSeq, true, A2($elm$core$List$map, _Debugger_init, value));
            if (tag === "Set_elm_builtin") return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$SetSeq, true, A3($elm$core$Set$foldr, _Debugger_initCons, _List_Nil, value));
            if (tag === "RBNode_elm_builtin" || tag == "RBEmpty_elm_builtin") return A2($elm$browser$Debugger$Expando$Dictionary, true, A3($elm$core$Dict$foldr, _Debugger_initKeyValueCons, _List_Nil, value));
            if (tag === "Array_elm_builtin") return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$ArraySeq, true, A3($elm$core$Array$foldr, _Debugger_initCons, _List_Nil, value));
            if (typeof tag === "number") return $elm$browser$Debugger$Expando$Primitive("<internals>");
            var char = tag.charCodeAt(0);
            if (char === 35 || 65 <= char && char <= 90) {
                var list = _List_Nil;
                for(var i in value){
                    if (i === "$") continue;
                    list = _List_Cons(_Debugger_init(value[i]), list);
                }
                return A3($elm$browser$Debugger$Expando$Constructor, char === 35 ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(tag), true, $elm$core$List$reverse(list));
            }
            return $elm$browser$Debugger$Expando$Primitive("<internals>");
        }
        if (typeof value === "object") {
            var dict = $elm$core$Dict$empty;
            for(var i in value)dict = A3($elm$core$Dict$insert, i, _Debugger_init(value[i]), dict);
            return A2($elm$browser$Debugger$Expando$Record, true, dict);
        }
        return $elm$browser$Debugger$Expando$Primitive("<internals>");
    }
    var _Debugger_initCons = F2(function initConsHelp(value, list) {
        return _List_Cons(_Debugger_init(value), list);
    });
    var _Debugger_initKeyValueCons = F3(function(key, value, list) {
        return _List_Cons(_Utils_Tuple2(_Debugger_init(key), _Debugger_init(value)), list);
    });
    function _Debugger_addSlashes(str, isChar) {
        var s = str.replace(/\\/g, "\\\\").replace(/\n/g, "\\n").replace(/\t/g, "\\t").replace(/\r/g, "\\r").replace(/\v/g, "\\v").replace(/\0/g, "\\0");
        if (isChar) return s.replace(/\'/g, "\\'");
        else return s.replace(/\"/g, '\\"');
    }
    // BLOCK EVENTS
    function _Debugger_updateBlocker(oldBlocker, newBlocker) {
        if (oldBlocker === newBlocker) return;
        var oldEvents = _Debugger_blockerToEvents(oldBlocker);
        var newEvents = _Debugger_blockerToEvents(newBlocker);
        // remove old blockers
        for(var i = 0; i < oldEvents.length; i++)document.removeEventListener(oldEvents[i], _Debugger_blocker, true);
        // add new blockers
        for(var i = 0; i < newEvents.length; i++)document.addEventListener(newEvents[i], _Debugger_blocker, true);
    }
    function _Debugger_blocker(event) {
        if (event.type === "keydown" && event.metaKey && event.which === 82) return;
        var isScroll = event.type === "scroll" || event.type === "wheel";
        for(var node = event.target; node; node = node.parentNode){
            if (isScroll ? node.id === "elm-debugger-details" : node.id === "elm-debugger-overlay") return;
        }
        event.stopPropagation();
        event.preventDefault();
    }
    function _Debugger_blockerToEvents(blocker) {
        return blocker === $elm$browser$Debugger$Overlay$BlockNone ? [] : blocker === $elm$browser$Debugger$Overlay$BlockMost ? _Debugger_mostEvents : _Debugger_allEvents;
    }
    var _Debugger_mostEvents = [
        "click",
        "dblclick",
        "mousemove",
        "mouseup",
        "mousedown",
        "mouseenter",
        "mouseleave",
        "touchstart",
        "touchend",
        "touchcancel",
        "touchmove",
        "pointerdown",
        "pointerup",
        "pointerover",
        "pointerout",
        "pointerenter",
        "pointerleave",
        "pointermove",
        "pointercancel",
        "dragstart",
        "drag",
        "dragend",
        "dragenter",
        "dragover",
        "dragleave",
        "drop",
        "keyup",
        "keydown",
        "keypress",
        "input",
        "change",
        "focus",
        "blur"
    ];
    var _Debugger_allEvents = _Debugger_mostEvents.concat("wheel", "scroll");
    // ELEMENT
    var _Debugger_element;
    var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args) {
        return _Platform_initialize(flagDecoder, args, impl.init, impl.update, impl.subscriptions, function(sendToApp, initialModel) {
            var view = impl.view;
            /**_UNUSED/
			var domNode = args['node'];
			//*/ /**/ var domNode = args && args["node"] ? args["node"] : _Debug_crash(0);
            //*/
            var currNode = _VirtualDom_virtualize(domNode);
            return _Browser_makeAnimator(initialModel, function(model) {
                var nextNode = view(model);
                var patches = _VirtualDom_diff(currNode, nextNode);
                domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
                currNode = nextNode;
            });
        });
    });
    // DOCUMENT
    var _Debugger_document;
    var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args) {
        return _Platform_initialize(flagDecoder, args, impl.init, impl.update, impl.subscriptions, function(sendToApp, initialModel) {
            var divertHrefToApp = impl.setup && impl.setup(sendToApp);
            var view = impl.view;
            var title = _VirtualDom_doc.title;
            var bodyNode = _VirtualDom_doc.body;
            var currNode = _VirtualDom_virtualize(bodyNode);
            return _Browser_makeAnimator(initialModel, function(model) {
                _VirtualDom_divertHrefToApp = divertHrefToApp;
                var doc = view(model);
                var nextNode = _VirtualDom_node("body")(_List_Nil)(doc.body);
                var patches = _VirtualDom_diff(currNode, nextNode);
                bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
                currNode = nextNode;
                _VirtualDom_divertHrefToApp = 0;
                title !== doc.title && (_VirtualDom_doc.title = title = doc.title);
            });
        });
    });
    // ANIMATION
    var _Browser_cancelAnimationFrame = typeof cancelAnimationFrame !== "undefined" ? cancelAnimationFrame : function(id) {
        clearTimeout(id);
    };
    var _Browser_requestAnimationFrame = typeof requestAnimationFrame !== "undefined" ? requestAnimationFrame : function(callback) {
        return setTimeout(callback, 1000 / 60);
    };
    function _Browser_makeAnimator(model, draw) {
        draw(model);
        var state = 0;
        function updateIfNeeded() {
            state = state === 1 ? 0 : (_Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1);
        }
        return function(nextModel, isSync) {
            model = nextModel;
            isSync ? (draw(model), state === 2 && (state = 1)) : (state === 0 && _Browser_requestAnimationFrame(updateIfNeeded), state = 2);
        };
    }
    // APPLICATION
    function _Browser_application(impl) {
        var onUrlChange = impl.onUrlChange;
        var onUrlRequest = impl.onUrlRequest;
        var key = function() {
            key.a(onUrlChange(_Browser_getUrl()));
        };
        return _Browser_document({
            setup: function(sendToApp) {
                key.a = sendToApp;
                _Browser_window.addEventListener("popstate", key);
                _Browser_window.navigator.userAgent.indexOf("Trident") < 0 || _Browser_window.addEventListener("hashchange", key);
                return F2(function(domNode, event) {
                    if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute("download")) {
                        event.preventDefault();
                        var href = domNode.href;
                        var curr = _Browser_getUrl();
                        var next = $elm$url$Url$fromString(href).a;
                        sendToApp(onUrlRequest(next && curr.protocol === next.protocol && curr.host === next.host && curr.port_.a === next.port_.a ? $elm$browser$Browser$Internal(next) : $elm$browser$Browser$External(href)));
                    }
                });
            },
            init: function(flags) {
                return A3(impl.init, flags, _Browser_getUrl(), key);
            },
            view: impl.view,
            update: impl.update,
            subscriptions: impl.subscriptions
        });
    }
    function _Browser_getUrl() {
        return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
    }
    var _Browser_go = F2(function(key, n) {
        return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
            n && history.go(n);
            key();
        }));
    });
    var _Browser_pushUrl = F2(function(key, url) {
        return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
            history.pushState({}, "", url);
            key();
        }));
    });
    var _Browser_replaceUrl = F2(function(key, url) {
        return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
            history.replaceState({}, "", url);
            key();
        }));
    });
    // GLOBAL EVENTS
    var _Browser_fakeNode = {
        addEventListener: function() {},
        removeEventListener: function() {}
    };
    var _Browser_doc = typeof document !== "undefined" ? document : _Browser_fakeNode;
    var _Browser_window = typeof window !== "undefined" ? window : _Browser_fakeNode;
    var _Browser_on = F3(function(node, eventName, sendToSelf) {
        return _Scheduler_spawn(_Scheduler_binding(function(callback) {
            function handler(event) {
                _Scheduler_rawSpawn(sendToSelf(event));
            }
            node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && {
                passive: true
            });
            return function() {
                node.removeEventListener(eventName, handler);
            };
        }));
    });
    var _Browser_decodeEvent = F2(function(decoder, event) {
        var result = _Json_runHelp(decoder, event);
        return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
    });
    // PAGE VISIBILITY
    function _Browser_visibilityInfo() {
        return typeof _VirtualDom_doc.hidden !== "undefined" ? {
            hidden: "hidden",
            change: "visibilitychange"
        } : typeof _VirtualDom_doc.mozHidden !== "undefined" ? {
            hidden: "mozHidden",
            change: "mozvisibilitychange"
        } : typeof _VirtualDom_doc.msHidden !== "undefined" ? {
            hidden: "msHidden",
            change: "msvisibilitychange"
        } : typeof _VirtualDom_doc.webkitHidden !== "undefined" ? {
            hidden: "webkitHidden",
            change: "webkitvisibilitychange"
        } : {
            hidden: "hidden",
            change: "visibilitychange"
        };
    }
    // ANIMATION FRAMES
    function _Browser_rAF() {
        return _Scheduler_binding(function(callback) {
            var id = _Browser_requestAnimationFrame(function() {
                callback(_Scheduler_succeed(Date.now()));
            });
            return function() {
                _Browser_cancelAnimationFrame(id);
            };
        });
    }
    function _Browser_now() {
        return _Scheduler_binding(function(callback) {
            callback(_Scheduler_succeed(Date.now()));
        });
    }
    // DOM STUFF
    function _Browser_withNode(id, doStuff) {
        return _Scheduler_binding(function(callback) {
            _Browser_requestAnimationFrame(function() {
                var node = document.getElementById(id);
                callback(node ? _Scheduler_succeed(doStuff(node)) : _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id)));
            });
        });
    }
    function _Browser_withWindow(doStuff) {
        return _Scheduler_binding(function(callback) {
            _Browser_requestAnimationFrame(function() {
                callback(_Scheduler_succeed(doStuff()));
            });
        });
    }
    // FOCUS and BLUR
    var _Browser_call = F2(function(functionName, id) {
        return _Browser_withNode(id, function(node) {
            node[functionName]();
            return _Utils_Tuple0;
        });
    });
    // WINDOW VIEWPORT
    function _Browser_getViewport() {
        return {
            scene: _Browser_getScene(),
            viewport: {
                x: _Browser_window.pageXOffset,
                y: _Browser_window.pageYOffset,
                width: _Browser_doc.documentElement.clientWidth,
                height: _Browser_doc.documentElement.clientHeight
            }
        };
    }
    function _Browser_getScene() {
        var body = _Browser_doc.body;
        var elem = _Browser_doc.documentElement;
        return {
            width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
            height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
        };
    }
    var _Browser_setViewport = F2(function(x, y) {
        return _Browser_withWindow(function() {
            _Browser_window.scroll(x, y);
            return _Utils_Tuple0;
        });
    });
    // ELEMENT VIEWPORT
    function _Browser_getViewportOf(id) {
        return _Browser_withNode(id, function(node) {
            return {
                scene: {
                    width: node.scrollWidth,
                    height: node.scrollHeight
                },
                viewport: {
                    x: node.scrollLeft,
                    y: node.scrollTop,
                    width: node.clientWidth,
                    height: node.clientHeight
                }
            };
        });
    }
    var _Browser_setViewportOf = F3(function(id, x, y) {
        return _Browser_withNode(id, function(node) {
            node.scrollLeft = x;
            node.scrollTop = y;
            return _Utils_Tuple0;
        });
    });
    // ELEMENT
    function _Browser_getElement(id) {
        return _Browser_withNode(id, function(node) {
            var rect = node.getBoundingClientRect();
            var x = _Browser_window.pageXOffset;
            var y = _Browser_window.pageYOffset;
            return {
                scene: _Browser_getScene(),
                viewport: {
                    x: x,
                    y: y,
                    width: _Browser_doc.documentElement.clientWidth,
                    height: _Browser_doc.documentElement.clientHeight
                },
                element: {
                    x: x + rect.left,
                    y: y + rect.top,
                    width: rect.width,
                    height: rect.height
                }
            };
        });
    }
    // LOAD and RELOAD
    function _Browser_reload(skipCache) {
        return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback) {
            _VirtualDom_doc.location.reload(skipCache);
        }));
    }
    function _Browser_load(url) {
        return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback) {
            try {
                _Browser_window.location = url;
            } catch (err) {
                // Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
                // Other browsers reload the page, so let's be consistent about that.
                _VirtualDom_doc.location.reload(false);
            }
        }));
    }
    var $elm$core$Basics$EQ = {
        $: "EQ"
    };
    var $elm$core$Basics$GT = {
        $: "GT"
    };
    var $elm$core$Basics$LT = {
        $: "LT"
    };
    var $elm$core$List$cons = _List_cons;
    var $elm$core$Dict$foldr = F3(function(func, acc, t) {
        foldr: while(true){
            if (t.$ === "RBEmpty_elm_builtin") return acc;
            else {
                var key = t.b;
                var value = t.c;
                var left = t.d;
                var right = t.e;
                var $temp$func = func, $temp$acc = A3(func, key, value, A3($elm$core$Dict$foldr, func, acc, right)), $temp$t = left;
                func = $temp$func;
                acc = $temp$acc;
                t = $temp$t;
                continue foldr;
            }
        }
    });
    var $elm$core$Dict$toList = function(dict) {
        return A3($elm$core$Dict$foldr, F3(function(key, value, list) {
            return A2($elm$core$List$cons, _Utils_Tuple2(key, value), list);
        }), _List_Nil, dict);
    };
    var $elm$core$Dict$keys = function(dict) {
        return A3($elm$core$Dict$foldr, F3(function(key, value, keyList) {
            return A2($elm$core$List$cons, key, keyList);
        }), _List_Nil, dict);
    };
    var $elm$core$Set$toList = function(_v0) {
        var dict = _v0.a;
        return $elm$core$Dict$keys(dict);
    };
    var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
    var $elm$core$Array$foldr = F3(function(func, baseCase, _v0) {
        var tree = _v0.c;
        var tail = _v0.d;
        var helper = F2(function(node, acc) {
            if (node.$ === "SubTree") {
                var subTree = node.a;
                return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
            } else {
                var values = node.a;
                return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
            }
        });
        return A3($elm$core$Elm$JsArray$foldr, helper, A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail), tree);
    });
    var $elm$core$Array$toList = function(array) {
        return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
    };
    var $elm$core$Result$Err = function(a) {
        return {
            $: "Err",
            a: a
        };
    };
    var $elm$json$Json$Decode$Failure = F2(function(a, b) {
        return {
            $: "Failure",
            a: a,
            b: b
        };
    });
    var $elm$json$Json$Decode$Field = F2(function(a, b) {
        return {
            $: "Field",
            a: a,
            b: b
        };
    });
    var $elm$json$Json$Decode$Index = F2(function(a, b) {
        return {
            $: "Index",
            a: a,
            b: b
        };
    });
    var $elm$core$Result$Ok = function(a) {
        return {
            $: "Ok",
            a: a
        };
    };
    var $elm$json$Json$Decode$OneOf = function(a) {
        return {
            $: "OneOf",
            a: a
        };
    };
    var $elm$core$Basics$False = {
        $: "False"
    };
    var $elm$core$Basics$add = _Basics_add;
    var $elm$core$Maybe$Just = function(a) {
        return {
            $: "Just",
            a: a
        };
    };
    var $elm$core$Maybe$Nothing = {
        $: "Nothing"
    };
    var $elm$core$String$all = _String_all;
    var $elm$core$Basics$and = _Basics_and;
    var $elm$core$Basics$append = _Utils_append;
    var $elm$json$Json$Encode$encode = _Json_encode;
    var $elm$core$String$fromInt = _String_fromNumber;
    var $elm$core$String$join = F2(function(sep, chunks) {
        return A2(_String_join, sep, _List_toArray(chunks));
    });
    var $elm$core$String$split = F2(function(sep, string) {
        return _List_fromArray(A2(_String_split, sep, string));
    });
    var $elm$json$Json$Decode$indent = function(str) {
        return A2($elm$core$String$join, "\n    ", A2($elm$core$String$split, "\n", str));
    };
    var $elm$core$List$foldl = F3(function(func, acc, list) {
        foldl: while(true){
            if (!list.b) return acc;
            else {
                var x = list.a;
                var xs = list.b;
                var $temp$func = func, $temp$acc = A2(func, x, acc), $temp$list = xs;
                func = $temp$func;
                acc = $temp$acc;
                list = $temp$list;
                continue foldl;
            }
        }
    });
    var $elm$core$List$length = function(xs) {
        return A3($elm$core$List$foldl, F2(function(_v0, i) {
            return i + 1;
        }), 0, xs);
    };
    var $elm$core$List$map2 = _List_map2;
    var $elm$core$Basics$le = _Utils_le;
    var $elm$core$Basics$sub = _Basics_sub;
    var $elm$core$List$rangeHelp = F3(function(lo, hi, list) {
        rangeHelp: while(true){
            if (_Utils_cmp(lo, hi) < 1) {
                var $temp$lo = lo, $temp$hi = hi - 1, $temp$list = A2($elm$core$List$cons, hi, list);
                lo = $temp$lo;
                hi = $temp$hi;
                list = $temp$list;
                continue rangeHelp;
            } else return list;
        }
    });
    var $elm$core$List$range = F2(function(lo, hi) {
        return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
    });
    var $elm$core$List$indexedMap = F2(function(f, xs) {
        return A3($elm$core$List$map2, f, A2($elm$core$List$range, 0, $elm$core$List$length(xs) - 1), xs);
    });
    var $elm$core$Char$toCode = _Char_toCode;
    var $elm$core$Char$isLower = function(_char) {
        var code = $elm$core$Char$toCode(_char);
        return 97 <= code && code <= 122;
    };
    var $elm$core$Char$isUpper = function(_char) {
        var code = $elm$core$Char$toCode(_char);
        return code <= 90 && 65 <= code;
    };
    var $elm$core$Basics$or = _Basics_or;
    var $elm$core$Char$isAlpha = function(_char) {
        return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
    };
    var $elm$core$Char$isDigit = function(_char) {
        var code = $elm$core$Char$toCode(_char);
        return code <= 57 && 48 <= code;
    };
    var $elm$core$Char$isAlphaNum = function(_char) {
        return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char);
    };
    var $elm$core$List$reverse = function(list) {
        return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
    };
    var $elm$core$String$uncons = _String_uncons;
    var $elm$json$Json$Decode$errorOneOf = F2(function(i, error) {
        return "\n\n(" + ($elm$core$String$fromInt(i + 1) + (") " + $elm$json$Json$Decode$indent($elm$json$Json$Decode$errorToString(error))));
    });
    var $elm$json$Json$Decode$errorToString = function(error) {
        return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
    };
    var $elm$json$Json$Decode$errorToStringHelp = F2(function(error, context) {
        errorToStringHelp: while(true)switch(error.$){
            case "Field":
                var f = error.a;
                var err = error.b;
                var isSimple = function() {
                    var _v1 = $elm$core$String$uncons(f);
                    if (_v1.$ === "Nothing") return false;
                    else {
                        var _v2 = _v1.a;
                        var _char = _v2.a;
                        var rest = _v2.b;
                        return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
                    }
                }();
                var fieldName = isSimple ? "." + f : "['" + (f + "']");
                var $temp$error = err, $temp$context = A2($elm$core$List$cons, fieldName, context);
                error = $temp$error;
                context = $temp$context;
                continue errorToStringHelp;
            case "Index":
                var i = error.a;
                var err = error.b;
                var indexName = "[" + ($elm$core$String$fromInt(i) + "]");
                var $temp$error = err, $temp$context = A2($elm$core$List$cons, indexName, context);
                error = $temp$error;
                context = $temp$context;
                continue errorToStringHelp;
            case "OneOf":
                var errors = error.a;
                if (!errors.b) return "Ran into a Json.Decode.oneOf with no possibilities" + function() {
                    if (!context.b) return "!";
                    else return " at json" + A2($elm$core$String$join, "", $elm$core$List$reverse(context));
                }();
                else if (!errors.b.b) {
                    var err = errors.a;
                    var $temp$error = err, $temp$context = context;
                    error = $temp$error;
                    context = $temp$context;
                    continue errorToStringHelp;
                } else {
                    var starter = function() {
                        if (!context.b) return "Json.Decode.oneOf";
                        else return "The Json.Decode.oneOf at json" + A2($elm$core$String$join, "", $elm$core$List$reverse(context));
                    }();
                    var introduction = starter + (" failed in the following " + ($elm$core$String$fromInt($elm$core$List$length(errors)) + " ways:"));
                    return A2($elm$core$String$join, "\n\n", A2($elm$core$List$cons, introduction, A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
                }
            default:
                var msg = error.a;
                var json = error.b;
                var introduction = function() {
                    if (!context.b) return "Problem with the given value:\n\n";
                    else return "Problem with the value at json" + (A2($elm$core$String$join, "", $elm$core$List$reverse(context)) + ":\n\n    ");
                }();
                return introduction + ($elm$json$Json$Decode$indent(A2($elm$json$Json$Encode$encode, 4, json)) + ("\n\n" + msg));
        }
    });
    var $elm$core$Array$branchFactor = 32;
    var $elm$core$Array$Array_elm_builtin = F4(function(a, b, c, d) {
        return {
            $: "Array_elm_builtin",
            a: a,
            b: b,
            c: c,
            d: d
        };
    });
    var $elm$core$Elm$JsArray$empty = _JsArray_empty;
    var $elm$core$Basics$ceiling = _Basics_ceiling;
    var $elm$core$Basics$fdiv = _Basics_fdiv;
    var $elm$core$Basics$logBase = F2(function(base, number) {
        return _Basics_log(number) / _Basics_log(base);
    });
    var $elm$core$Basics$toFloat = _Basics_toFloat;
    var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
    var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
    var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
    var $elm$core$Array$Leaf = function(a) {
        return {
            $: "Leaf",
            a: a
        };
    };
    var $elm$core$Basics$apL = F2(function(f, x) {
        return f(x);
    });
    var $elm$core$Basics$apR = F2(function(x, f) {
        return f(x);
    });
    var $elm$core$Basics$eq = _Utils_equal;
    var $elm$core$Basics$floor = _Basics_floor;
    var $elm$core$Elm$JsArray$length = _JsArray_length;
    var $elm$core$Basics$gt = _Utils_gt;
    var $elm$core$Basics$max = F2(function(x, y) {
        return _Utils_cmp(x, y) > 0 ? x : y;
    });
    var $elm$core$Basics$mul = _Basics_mul;
    var $elm$core$Array$SubTree = function(a) {
        return {
            $: "SubTree",
            a: a
        };
    };
    var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
    var $elm$core$Array$compressNodes = F2(function(nodes, acc) {
        compressNodes: while(true){
            var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
            var node = _v0.a;
            var remainingNodes = _v0.b;
            var newAcc = A2($elm$core$List$cons, $elm$core$Array$SubTree(node), acc);
            if (!remainingNodes.b) return $elm$core$List$reverse(newAcc);
            else {
                var $temp$nodes = remainingNodes, $temp$acc = newAcc;
                nodes = $temp$nodes;
                acc = $temp$acc;
                continue compressNodes;
            }
        }
    });
    var $elm$core$Tuple$first = function(_v0) {
        var x = _v0.a;
        return x;
    };
    var $elm$core$Array$treeFromBuilder = F2(function(nodeList, nodeListSize) {
        treeFromBuilder: while(true){
            var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
            if (newNodeSize === 1) return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
            else {
                var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil), $temp$nodeListSize = newNodeSize;
                nodeList = $temp$nodeList;
                nodeListSize = $temp$nodeListSize;
                continue treeFromBuilder;
            }
        }
    });
    var $elm$core$Array$builderToArray = F2(function(reverseNodeList, builder) {
        if (!builder.nodeListSize) return A4($elm$core$Array$Array_elm_builtin, $elm$core$Elm$JsArray$length(builder.tail), $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, builder.tail);
        else {
            var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
            var depth = $elm$core$Basics$floor(A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
            var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
            var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
            return A4($elm$core$Array$Array_elm_builtin, $elm$core$Elm$JsArray$length(builder.tail) + treeLen, A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep), tree, builder.tail);
        }
    });
    var $elm$core$Basics$idiv = _Basics_idiv;
    var $elm$core$Basics$lt = _Utils_lt;
    var $elm$core$Array$initializeHelp = F5(function(fn, fromIndex, len, nodeList, tail) {
        initializeHelp: while(true){
            if (fromIndex < 0) return A2($elm$core$Array$builderToArray, false, {
                nodeList: nodeList,
                nodeListSize: len / $elm$core$Array$branchFactor | 0,
                tail: tail
            });
            else {
                var leaf = $elm$core$Array$Leaf(A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
                var $temp$fn = fn, $temp$fromIndex = fromIndex - $elm$core$Array$branchFactor, $temp$len = len, $temp$nodeList = A2($elm$core$List$cons, leaf, nodeList), $temp$tail = tail;
                fn = $temp$fn;
                fromIndex = $temp$fromIndex;
                len = $temp$len;
                nodeList = $temp$nodeList;
                tail = $temp$tail;
                continue initializeHelp;
            }
        }
    });
    var $elm$core$Basics$remainderBy = _Basics_remainderBy;
    var $elm$core$Array$initialize = F2(function(len, fn) {
        if (len <= 0) return $elm$core$Array$empty;
        else {
            var tailLen = len % $elm$core$Array$branchFactor;
            var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
            var initialFromIndex = len - tailLen - $elm$core$Array$branchFactor;
            return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
        }
    });
    var $elm$core$Basics$True = {
        $: "True"
    };
    var $elm$core$Result$isOk = function(result) {
        if (result.$ === "Ok") return true;
        else return false;
    };
    var $elm$json$Json$Decode$map = _Json_map1;
    var $elm$json$Json$Decode$map2 = _Json_map2;
    var $elm$json$Json$Decode$succeed = _Json_succeed;
    var $elm$virtual_dom$VirtualDom$toHandlerInt = function(handler) {
        switch(handler.$){
            case "Normal":
                return 0;
            case "MayStopPropagation":
                return 1;
            case "MayPreventDefault":
                return 2;
            default:
                return 3;
        }
    };
    var $elm$browser$Debugger$Expando$ArraySeq = {
        $: "ArraySeq"
    };
    var $elm$browser$Debugger$Overlay$BlockMost = {
        $: "BlockMost"
    };
    var $elm$browser$Debugger$Overlay$BlockNone = {
        $: "BlockNone"
    };
    var $elm$browser$Debugger$Expando$Constructor = F3(function(a, b, c) {
        return {
            $: "Constructor",
            a: a,
            b: b,
            c: c
        };
    });
    var $elm$browser$Debugger$Expando$Dictionary = F2(function(a, b) {
        return {
            $: "Dictionary",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$Main$Down = {
        $: "Down"
    };
    var $elm$browser$Debugger$Expando$ListSeq = {
        $: "ListSeq"
    };
    var $elm$browser$Debugger$Main$NoOp = {
        $: "NoOp"
    };
    var $elm$browser$Debugger$Expando$Primitive = function(a) {
        return {
            $: "Primitive",
            a: a
        };
    };
    var $elm$browser$Debugger$Expando$Record = F2(function(a, b) {
        return {
            $: "Record",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$Expando$S = function(a) {
        return {
            $: "S",
            a: a
        };
    };
    var $elm$browser$Debugger$Expando$Sequence = F3(function(a, b, c) {
        return {
            $: "Sequence",
            a: a,
            b: b,
            c: c
        };
    });
    var $elm$browser$Debugger$Expando$SetSeq = {
        $: "SetSeq"
    };
    var $elm$browser$Debugger$Main$Up = {
        $: "Up"
    };
    var $elm$browser$Debugger$Main$UserMsg = function(a) {
        return {
            $: "UserMsg",
            a: a
        };
    };
    var $elm$browser$Debugger$Main$Export = {
        $: "Export"
    };
    var $elm$browser$Debugger$Main$Import = {
        $: "Import"
    };
    var $elm$browser$Debugger$Main$Open = {
        $: "Open"
    };
    var $elm$browser$Debugger$Main$OverlayMsg = function(a) {
        return {
            $: "OverlayMsg",
            a: a
        };
    };
    var $elm$browser$Debugger$Main$Resume = {
        $: "Resume"
    };
    var $elm$browser$Debugger$Main$isPaused = function(state) {
        if (state.$ === "Running") return false;
        else return true;
    };
    var $elm$browser$Debugger$History$size = function(history1) {
        return history1.numMessages;
    };
    var $elm$browser$Debugger$Overlay$Accept = function(a) {
        return {
            $: "Accept",
            a: a
        };
    };
    var $elm$browser$Debugger$Overlay$Choose = F2(function(a, b) {
        return {
            $: "Choose",
            a: a,
            b: b
        };
    });
    var $elm$html$Html$div = _VirtualDom_node("div");
    var $elm$json$Json$Encode$string = _Json_wrap;
    var $elm$html$Html$Attributes$stringProperty = F2(function(key, string) {
        return A2(_VirtualDom_property, key, $elm$json$Json$Encode$string(string));
    });
    var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty("id");
    var $elm$virtual_dom$VirtualDom$Normal = function(a) {
        return {
            $: "Normal",
            a: a
        };
    };
    var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
    var $elm$html$Html$Events$on = F2(function(event, decoder) {
        return A2($elm$virtual_dom$VirtualDom$on, event, $elm$virtual_dom$VirtualDom$Normal(decoder));
    });
    var $elm$html$Html$Events$onClick = function(msg) {
        return A2($elm$html$Html$Events$on, "click", $elm$json$Json$Decode$succeed(msg));
    };
    var $elm$html$Html$span = _VirtualDom_node("span");
    var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
    var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
    var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
    var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
    var $elm$html$Html$a = _VirtualDom_node("a");
    var $elm$browser$Debugger$Overlay$goodNews1 = "\nThe good news is that having values like this in your message type is not\nso great in the long run. You are better off using simpler data, like\n";
    var $elm$browser$Debugger$Overlay$goodNews2 = "\nfunction can pattern match on that data and call whatever functions, JSON\ndecoders, etc. you need. This makes the code much more explicit and easy to\nfollow for other readers (or you in a few months!)\n";
    var $elm$html$Html$Attributes$href = function(url) {
        return A2($elm$html$Html$Attributes$stringProperty, "href", _VirtualDom_noJavaScriptUri(url));
    };
    var $elm$core$List$foldrHelper = F4(function(fn, acc, ctr, ls) {
        if (!ls.b) return acc;
        else {
            var a = ls.a;
            var r1 = ls.b;
            if (!r1.b) return A2(fn, a, acc);
            else {
                var b = r1.a;
                var r2 = r1.b;
                if (!r2.b) return A2(fn, a, A2(fn, b, acc));
                else {
                    var c = r2.a;
                    var r3 = r2.b;
                    if (!r3.b) return A2(fn, a, A2(fn, b, A2(fn, c, acc)));
                    else {
                        var d = r3.a;
                        var r4 = r3.b;
                        var res = ctr > 500 ? A3($elm$core$List$foldl, fn, acc, $elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
                        return A2(fn, a, A2(fn, b, A2(fn, c, A2(fn, d, res))));
                    }
                }
            }
        }
    });
    var $elm$core$List$foldr = F3(function(fn, acc, ls) {
        return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
    });
    var $elm$core$List$map = F2(function(f, xs) {
        return A3($elm$core$List$foldr, F2(function(x, acc) {
            return A2($elm$core$List$cons, f(x), acc);
        }), _List_Nil, xs);
    });
    var $elm$html$Html$p = _VirtualDom_node("p");
    var $elm$html$Html$ul = _VirtualDom_node("ul");
    var $elm$html$Html$code = _VirtualDom_node("code");
    var $elm$browser$Debugger$Overlay$viewCode = function(name) {
        return A2($elm$html$Html$code, _List_Nil, _List_fromArray([
            $elm$html$Html$text(name)
        ]));
    };
    var $elm$browser$Debugger$Overlay$addCommas = function(items) {
        if (!items.b) return "";
        else {
            if (!items.b.b) {
                var item = items.a;
                return item;
            } else if (!items.b.b.b) {
                var item1 = items.a;
                var _v1 = items.b;
                var item2 = _v1.a;
                return item1 + (" and " + item2);
            } else {
                var lastItem = items.a;
                var otherItems = items.b;
                return A2($elm$core$String$join, ", ", _Utils_ap(otherItems, _List_fromArray([
                    " and " + lastItem
                ])));
            }
        }
    };
    var $elm$html$Html$li = _VirtualDom_node("li");
    var $elm$browser$Debugger$Overlay$problemToString = function(problem) {
        switch(problem.$){
            case "Function":
                return "functions";
            case "Decoder":
                return "JSON decoders";
            case "Task":
                return "tasks";
            case "Process":
                return "processes";
            case "Socket":
                return "web sockets";
            case "Request":
                return "HTTP requests";
            case "Program":
                return "programs";
            default:
                return "virtual DOM values";
        }
    };
    var $elm$browser$Debugger$Overlay$viewProblemType = function(_v0) {
        var name = _v0.name;
        var problems = _v0.problems;
        return A2($elm$html$Html$li, _List_Nil, _List_fromArray([
            $elm$browser$Debugger$Overlay$viewCode(name),
            $elm$html$Html$text(" can contain " + ($elm$browser$Debugger$Overlay$addCommas(A2($elm$core$List$map, $elm$browser$Debugger$Overlay$problemToString, problems)) + "."))
        ]));
    };
    var $elm$browser$Debugger$Overlay$viewBadMetadata = function(_v0) {
        var message = _v0.message;
        var problems = _v0.problems;
        return _List_fromArray([
            A2($elm$html$Html$p, _List_Nil, _List_fromArray([
                $elm$html$Html$text("The "),
                $elm$browser$Debugger$Overlay$viewCode(message),
                $elm$html$Html$text(" type of your program cannot be reliably serialized for history files.")
            ])),
            A2($elm$html$Html$p, _List_Nil, _List_fromArray([
                $elm$html$Html$text("Functions cannot be serialized, nor can values that contain functions. This is a problem in these places:")
            ])),
            A2($elm$html$Html$ul, _List_Nil, A2($elm$core$List$map, $elm$browser$Debugger$Overlay$viewProblemType, problems)),
            A2($elm$html$Html$p, _List_Nil, _List_fromArray([
                $elm$html$Html$text($elm$browser$Debugger$Overlay$goodNews1),
                A2($elm$html$Html$a, _List_fromArray([
                    $elm$html$Html$Attributes$href("https://guide.elm-lang.org/types/custom_types.html")
                ]), _List_fromArray([
                    $elm$html$Html$text("custom types")
                ])),
                $elm$html$Html$text(", in your messages. From there, your "),
                $elm$browser$Debugger$Overlay$viewCode("update"),
                $elm$html$Html$text($elm$browser$Debugger$Overlay$goodNews2)
            ]))
        ]);
    };
    var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
    var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
    var $elm$browser$Debugger$Overlay$Cancel = {
        $: "Cancel"
    };
    var $elm$browser$Debugger$Overlay$Proceed = {
        $: "Proceed"
    };
    var $elm$html$Html$button = _VirtualDom_node("button");
    var $elm$browser$Debugger$Overlay$viewButtons = function(buttons) {
        var btn = F2(function(msg, string) {
            return A2($elm$html$Html$button, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "margin-right", "20px"),
                $elm$html$Html$Events$onClick(msg)
            ]), _List_fromArray([
                $elm$html$Html$text(string)
            ]));
        });
        var buttonNodes = function() {
            if (buttons.$ === "Accept") {
                var proceed = buttons.a;
                return _List_fromArray([
                    A2(btn, $elm$browser$Debugger$Overlay$Proceed, proceed)
                ]);
            } else {
                var cancel = buttons.a;
                var proceed = buttons.b;
                return _List_fromArray([
                    A2(btn, $elm$browser$Debugger$Overlay$Cancel, cancel),
                    A2(btn, $elm$browser$Debugger$Overlay$Proceed, proceed)
                ]);
            }
        }();
        return A2($elm$html$Html$div, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "height", "60px"),
            A2($elm$html$Html$Attributes$style, "line-height", "60px"),
            A2($elm$html$Html$Attributes$style, "text-align", "right"),
            A2($elm$html$Html$Attributes$style, "background-color", "rgb(50, 50, 50)")
        ]), buttonNodes);
    };
    var $elm$browser$Debugger$Overlay$viewMessage = F4(function(config, title, details, buttons) {
        return A2($elm$html$Html$div, _List_fromArray([
            $elm$html$Html$Attributes$id("elm-debugger-overlay"),
            A2($elm$html$Html$Attributes$style, "position", "fixed"),
            A2($elm$html$Html$Attributes$style, "top", "0"),
            A2($elm$html$Html$Attributes$style, "left", "0"),
            A2($elm$html$Html$Attributes$style, "width", "100vw"),
            A2($elm$html$Html$Attributes$style, "height", "100vh"),
            A2($elm$html$Html$Attributes$style, "color", "white"),
            A2($elm$html$Html$Attributes$style, "pointer-events", "none"),
            A2($elm$html$Html$Attributes$style, "font-family", "'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif"),
            A2($elm$html$Html$Attributes$style, "z-index", "2147483647")
        ]), _List_fromArray([
            A2($elm$html$Html$div, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "position", "absolute"),
                A2($elm$html$Html$Attributes$style, "width", "600px"),
                A2($elm$html$Html$Attributes$style, "height", "100vh"),
                A2($elm$html$Html$Attributes$style, "padding-left", "calc(50% - 300px)"),
                A2($elm$html$Html$Attributes$style, "padding-right", "calc(50% - 300px)"),
                A2($elm$html$Html$Attributes$style, "background-color", "rgba(200, 200, 200, 0.7)"),
                A2($elm$html$Html$Attributes$style, "pointer-events", "auto")
            ]), _List_fromArray([
                A2($elm$html$Html$div, _List_fromArray([
                    A2($elm$html$Html$Attributes$style, "font-size", "36px"),
                    A2($elm$html$Html$Attributes$style, "height", "80px"),
                    A2($elm$html$Html$Attributes$style, "background-color", "rgb(50, 50, 50)"),
                    A2($elm$html$Html$Attributes$style, "padding-left", "22px"),
                    A2($elm$html$Html$Attributes$style, "vertical-align", "middle"),
                    A2($elm$html$Html$Attributes$style, "line-height", "80px")
                ]), _List_fromArray([
                    $elm$html$Html$text(title)
                ])),
                A2($elm$html$Html$div, _List_fromArray([
                    $elm$html$Html$Attributes$id("elm-debugger-details"),
                    A2($elm$html$Html$Attributes$style, "padding", " 8px 20px"),
                    A2($elm$html$Html$Attributes$style, "overflow-y", "auto"),
                    A2($elm$html$Html$Attributes$style, "max-height", "calc(100vh - 156px)"),
                    A2($elm$html$Html$Attributes$style, "background-color", "rgb(61, 61, 61)")
                ]), details),
                A2($elm$html$Html$map, config.wrap, $elm$browser$Debugger$Overlay$viewButtons(buttons))
            ]))
        ]));
    });
    var $elm$virtual_dom$VirtualDom$attribute = F2(function(key, value) {
        return A2(_VirtualDom_attribute, _VirtualDom_noOnOrFormAction(key), _VirtualDom_noJavaScriptOrHtmlUri(value));
    });
    var $elm$core$Basics$negate = function(n) {
        return -n;
    };
    var $elm$virtual_dom$VirtualDom$nodeNS = function(tag) {
        return _VirtualDom_nodeNS(_VirtualDom_noScript(tag));
    };
    var $elm$core$String$fromFloat = _String_fromNumber;
    var $elm$browser$Debugger$Overlay$viewShape = F4(function(x, y, angle, coordinates) {
        return A4($elm$virtual_dom$VirtualDom$nodeNS, "http://www.w3.org/2000/svg", "polygon", _List_fromArray([
            A2($elm$virtual_dom$VirtualDom$attribute, "points", coordinates),
            A2($elm$virtual_dom$VirtualDom$attribute, "transform", "translate(" + ($elm$core$String$fromFloat(x) + (" " + ($elm$core$String$fromFloat(y) + (") rotate(" + ($elm$core$String$fromFloat(-angle) + ")"))))))
        ]), _List_Nil);
    });
    var $elm$browser$Debugger$Overlay$elmLogo = A4($elm$virtual_dom$VirtualDom$nodeNS, "http://www.w3.org/2000/svg", "svg", _List_fromArray([
        A2($elm$virtual_dom$VirtualDom$attribute, "viewBox", "-300 -300 600 600"),
        A2($elm$virtual_dom$VirtualDom$attribute, "xmlns", "http://www.w3.org/2000/svg"),
        A2($elm$virtual_dom$VirtualDom$attribute, "fill", "currentColor"),
        A2($elm$virtual_dom$VirtualDom$attribute, "width", "24px"),
        A2($elm$virtual_dom$VirtualDom$attribute, "height", "24px")
    ]), _List_fromArray([
        A4($elm$virtual_dom$VirtualDom$nodeNS, "http://www.w3.org/2000/svg", "g", _List_fromArray([
            A2($elm$virtual_dom$VirtualDom$attribute, "transform", "scale(1 -1)")
        ]), _List_fromArray([
            A4($elm$browser$Debugger$Overlay$viewShape, 0, -210, 0, "-280,-90 0,190 280,-90"),
            A4($elm$browser$Debugger$Overlay$viewShape, -210, 0, 90, "-280,-90 0,190 280,-90"),
            A4($elm$browser$Debugger$Overlay$viewShape, 207, 207, 45, "-198,-66 0,132 198,-66"),
            A4($elm$browser$Debugger$Overlay$viewShape, 150, 0, 0, "-130,0 0,-130 130,0 0,130"),
            A4($elm$browser$Debugger$Overlay$viewShape, -89, 239, 0, "-191,61 69,61 191,-61 -69,-61"),
            A4($elm$browser$Debugger$Overlay$viewShape, 0, 106, 180, "-130,-44 0,86  130,-44"),
            A4($elm$browser$Debugger$Overlay$viewShape, 256, -150, 270, "-130,-44 0,86  130,-44")
        ]))
    ]));
    var $elm$core$String$length = _String_length;
    var $elm$browser$Debugger$Overlay$viewMiniControls = F2(function(config, numMsgs) {
        var string = $elm$core$String$fromInt(numMsgs);
        var width = $elm$core$String$fromInt(2 + $elm$core$String$length(string));
        return A2($elm$html$Html$div, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "position", "fixed"),
            A2($elm$html$Html$Attributes$style, "bottom", "2em"),
            A2($elm$html$Html$Attributes$style, "right", "2em"),
            A2($elm$html$Html$Attributes$style, "width", "calc(42px + " + (width + "ch)")),
            A2($elm$html$Html$Attributes$style, "height", "36px"),
            A2($elm$html$Html$Attributes$style, "background-color", "#1293D8"),
            A2($elm$html$Html$Attributes$style, "color", "white"),
            A2($elm$html$Html$Attributes$style, "font-family", "monospace"),
            A2($elm$html$Html$Attributes$style, "pointer-events", "auto"),
            A2($elm$html$Html$Attributes$style, "z-index", "2147483647"),
            A2($elm$html$Html$Attributes$style, "display", "flex"),
            A2($elm$html$Html$Attributes$style, "justify-content", "center"),
            A2($elm$html$Html$Attributes$style, "align-items", "center"),
            A2($elm$html$Html$Attributes$style, "cursor", "pointer"),
            $elm$html$Html$Events$onClick(config.open)
        ]), _List_fromArray([
            $elm$browser$Debugger$Overlay$elmLogo,
            A2($elm$html$Html$span, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "padding-left", "calc(1ch + 6px)"),
                A2($elm$html$Html$Attributes$style, "padding-right", "1ch")
            ]), _List_fromArray([
                $elm$html$Html$text(string)
            ]))
        ]));
    });
    var $elm$browser$Debugger$Overlay$explanationBad = "\nThe messages in this history do not match the messages handled by your\nprogram. I noticed changes in the following types:\n";
    var $elm$browser$Debugger$Overlay$explanationRisky = "\nThis history seems old. It will work with this program, but some\nmessages have been added since the history was created:\n";
    var $elm$core$List$intersperse = F2(function(sep, xs) {
        if (!xs.b) return _List_Nil;
        else {
            var hd = xs.a;
            var tl = xs.b;
            var step = F2(function(x, rest) {
                return A2($elm$core$List$cons, sep, A2($elm$core$List$cons, x, rest));
            });
            var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
            return A2($elm$core$List$cons, hd, spersed);
        }
    });
    var $elm$browser$Debugger$Overlay$viewMention = F2(function(tags, verbed) {
        var _v0 = A2($elm$core$List$map, $elm$browser$Debugger$Overlay$viewCode, $elm$core$List$reverse(tags));
        if (!_v0.b) return $elm$html$Html$text("");
        else {
            if (!_v0.b.b) {
                var tag = _v0.a;
                return A2($elm$html$Html$li, _List_Nil, _List_fromArray([
                    $elm$html$Html$text(verbed),
                    tag,
                    $elm$html$Html$text(".")
                ]));
            } else if (!_v0.b.b.b) {
                var tag2 = _v0.a;
                var _v1 = _v0.b;
                var tag1 = _v1.a;
                return A2($elm$html$Html$li, _List_Nil, _List_fromArray([
                    $elm$html$Html$text(verbed),
                    tag1,
                    $elm$html$Html$text(" and "),
                    tag2,
                    $elm$html$Html$text(".")
                ]));
            } else {
                var lastTag = _v0.a;
                var otherTags = _v0.b;
                return A2($elm$html$Html$li, _List_Nil, A2($elm$core$List$cons, $elm$html$Html$text(verbed), _Utils_ap(A2($elm$core$List$intersperse, $elm$html$Html$text(", "), $elm$core$List$reverse(otherTags)), _List_fromArray([
                    $elm$html$Html$text(", and "),
                    lastTag,
                    $elm$html$Html$text(".")
                ]))));
            }
        }
    });
    var $elm$browser$Debugger$Overlay$viewChange = function(change) {
        return A2($elm$html$Html$li, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "margin", "8px 0")
        ]), function() {
            if (change.$ === "AliasChange") {
                var name = change.a;
                return _List_fromArray([
                    A2($elm$html$Html$span, _List_fromArray([
                        A2($elm$html$Html$Attributes$style, "font-size", "1.5em")
                    ]), _List_fromArray([
                        $elm$browser$Debugger$Overlay$viewCode(name)
                    ]))
                ]);
            } else {
                var name = change.a;
                var removed = change.b.removed;
                var changed = change.b.changed;
                var added = change.b.added;
                var argsMatch = change.b.argsMatch;
                return _List_fromArray([
                    A2($elm$html$Html$span, _List_fromArray([
                        A2($elm$html$Html$Attributes$style, "font-size", "1.5em")
                    ]), _List_fromArray([
                        $elm$browser$Debugger$Overlay$viewCode(name)
                    ])),
                    A2($elm$html$Html$ul, _List_fromArray([
                        A2($elm$html$Html$Attributes$style, "list-style-type", "disc"),
                        A2($elm$html$Html$Attributes$style, "padding-left", "2em")
                    ]), _List_fromArray([
                        A2($elm$browser$Debugger$Overlay$viewMention, removed, "Removed "),
                        A2($elm$browser$Debugger$Overlay$viewMention, changed, "Changed "),
                        A2($elm$browser$Debugger$Overlay$viewMention, added, "Added ")
                    ])),
                    argsMatch ? $elm$html$Html$text("") : $elm$html$Html$text("This may be due to the fact that the type variable names changed.")
                ]);
            }
        }());
    };
    var $elm$browser$Debugger$Overlay$viewReport = F2(function(isBad, report) {
        switch(report.$){
            case "CorruptHistory":
                return _List_fromArray([
                    $elm$html$Html$text("Looks like this history file is corrupt. I cannot understand it.")
                ]);
            case "VersionChanged":
                var old = report.a;
                var _new = report.b;
                return _List_fromArray([
                    $elm$html$Html$text("This history was created with Elm " + (old + (", but you are using Elm " + (_new + " right now."))))
                ]);
            case "MessageChanged":
                var old = report.a;
                var _new = report.b;
                return _List_fromArray([
                    $elm$html$Html$text("To import some other history, the overall message type must be the same. The old history has "),
                    $elm$browser$Debugger$Overlay$viewCode(old),
                    $elm$html$Html$text(" messages, but the new program works with "),
                    $elm$browser$Debugger$Overlay$viewCode(_new),
                    $elm$html$Html$text(" messages.")
                ]);
            default:
                var changes = report.a;
                return _List_fromArray([
                    A2($elm$html$Html$p, _List_Nil, _List_fromArray([
                        $elm$html$Html$text(isBad ? $elm$browser$Debugger$Overlay$explanationBad : $elm$browser$Debugger$Overlay$explanationRisky)
                    ])),
                    A2($elm$html$Html$ul, _List_fromArray([
                        A2($elm$html$Html$Attributes$style, "list-style-type", "none"),
                        A2($elm$html$Html$Attributes$style, "padding-left", "20px")
                    ]), A2($elm$core$List$map, $elm$browser$Debugger$Overlay$viewChange, changes))
                ]);
        }
    });
    var $elm$browser$Debugger$Overlay$view = F5(function(config, isPaused, isOpen, numMsgs, state) {
        switch(state.$){
            case "None":
                return isOpen ? $elm$html$Html$text("") : isPaused ? A2($elm$html$Html$div, _List_fromArray([
                    $elm$html$Html$Attributes$id("elm-debugger-overlay"),
                    A2($elm$html$Html$Attributes$style, "position", "fixed"),
                    A2($elm$html$Html$Attributes$style, "top", "0"),
                    A2($elm$html$Html$Attributes$style, "left", "0"),
                    A2($elm$html$Html$Attributes$style, "width", "100vw"),
                    A2($elm$html$Html$Attributes$style, "height", "100vh"),
                    A2($elm$html$Html$Attributes$style, "cursor", "pointer"),
                    A2($elm$html$Html$Attributes$style, "display", "flex"),
                    A2($elm$html$Html$Attributes$style, "align-items", "center"),
                    A2($elm$html$Html$Attributes$style, "justify-content", "center"),
                    A2($elm$html$Html$Attributes$style, "pointer-events", "auto"),
                    A2($elm$html$Html$Attributes$style, "background-color", "rgba(200, 200, 200, 0.7)"),
                    A2($elm$html$Html$Attributes$style, "color", "white"),
                    A2($elm$html$Html$Attributes$style, "font-family", "'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif"),
                    A2($elm$html$Html$Attributes$style, "z-index", "2147483646"),
                    $elm$html$Html$Events$onClick(config.resume)
                ]), _List_fromArray([
                    A2($elm$html$Html$span, _List_fromArray([
                        A2($elm$html$Html$Attributes$style, "font-size", "80px")
                    ]), _List_fromArray([
                        $elm$html$Html$text("Click to Resume")
                    ])),
                    A2($elm$browser$Debugger$Overlay$viewMiniControls, config, numMsgs)
                ])) : A2($elm$browser$Debugger$Overlay$viewMiniControls, config, numMsgs);
            case "BadMetadata":
                var badMetadata_ = state.a;
                return A4($elm$browser$Debugger$Overlay$viewMessage, config, "Cannot use Import or Export", $elm$browser$Debugger$Overlay$viewBadMetadata(badMetadata_), $elm$browser$Debugger$Overlay$Accept("Ok"));
            case "BadImport":
                var report = state.a;
                return A4($elm$browser$Debugger$Overlay$viewMessage, config, "Cannot Import History", A2($elm$browser$Debugger$Overlay$viewReport, true, report), $elm$browser$Debugger$Overlay$Accept("Ok"));
            default:
                var report = state.a;
                return A4($elm$browser$Debugger$Overlay$viewMessage, config, "Warning", A2($elm$browser$Debugger$Overlay$viewReport, false, report), A2($elm$browser$Debugger$Overlay$Choose, "Cancel", "Import Anyway"));
        }
    });
    var $elm$browser$Debugger$Main$cornerView = function(model) {
        return A5($elm$browser$Debugger$Overlay$view, {
            exportHistory: $elm$browser$Debugger$Main$Export,
            importHistory: $elm$browser$Debugger$Main$Import,
            open: $elm$browser$Debugger$Main$Open,
            resume: $elm$browser$Debugger$Main$Resume,
            wrap: $elm$browser$Debugger$Main$OverlayMsg
        }, $elm$browser$Debugger$Main$isPaused(model.state), _Debugger_isOpen(model.popout), $elm$browser$Debugger$History$size(model.history), model.overlay);
    };
    var $elm$core$Dict$RBEmpty_elm_builtin = {
        $: "RBEmpty_elm_builtin"
    };
    var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
    var $elm$core$Set$foldr = F3(function(func, initialState, _v0) {
        var dict = _v0.a;
        return A3($elm$core$Dict$foldr, F3(function(key, _v1, state) {
            return A2(func, key, state);
        }), initialState, dict);
    });
    var $elm$browser$Debugger$Main$getCurrentModel = function(state) {
        if (state.$ === "Running") {
            var model = state.a;
            return model;
        } else {
            var model = state.b;
            return model;
        }
    };
    var $elm$browser$Debugger$Main$getUserModel = function(model) {
        return $elm$browser$Debugger$Main$getCurrentModel(model.state);
    };
    var $elm$browser$Debugger$Main$initialWindowHeight = 420;
    var $elm$browser$Debugger$Main$initialWindowWidth = 900;
    var $elm$core$Dict$Black = {
        $: "Black"
    };
    var $elm$core$Dict$RBNode_elm_builtin = F5(function(a, b, c, d, e) {
        return {
            $: "RBNode_elm_builtin",
            a: a,
            b: b,
            c: c,
            d: d,
            e: e
        };
    });
    var $elm$core$Dict$Red = {
        $: "Red"
    };
    var $elm$core$Dict$balance = F5(function(color, key, value, left, right) {
        if (right.$ === "RBNode_elm_builtin" && right.a.$ === "Red") {
            var _v1 = right.a;
            var rK = right.b;
            var rV = right.c;
            var rLeft = right.d;
            var rRight = right.e;
            if (left.$ === "RBNode_elm_builtin" && left.a.$ === "Red") {
                var _v3 = left.a;
                var lK = left.b;
                var lV = left.c;
                var lLeft = left.d;
                var lRight = left.e;
                return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
            } else return A5($elm$core$Dict$RBNode_elm_builtin, color, rK, rV, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft), rRight);
        } else {
            if (left.$ === "RBNode_elm_builtin" && left.a.$ === "Red" && left.d.$ === "RBNode_elm_builtin" && left.d.a.$ === "Red") {
                var _v5 = left.a;
                var lK = left.b;
                var lV = left.c;
                var _v6 = left.d;
                var _v7 = _v6.a;
                var llK = _v6.b;
                var llV = _v6.c;
                var llLeft = _v6.d;
                var llRight = _v6.e;
                var lRight = left.e;
                return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
            } else return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
        }
    });
    var $elm$core$Basics$compare = _Utils_compare;
    var $elm$core$Dict$insertHelp = F3(function(key, value, dict) {
        if (dict.$ === "RBEmpty_elm_builtin") return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
        else {
            var nColor = dict.a;
            var nKey = dict.b;
            var nValue = dict.c;
            var nLeft = dict.d;
            var nRight = dict.e;
            var _v1 = A2($elm$core$Basics$compare, key, nKey);
            switch(_v1.$){
                case "LT":
                    return A5($elm$core$Dict$balance, nColor, nKey, nValue, A3($elm$core$Dict$insertHelp, key, value, nLeft), nRight);
                case "EQ":
                    return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
                default:
                    return A5($elm$core$Dict$balance, nColor, nKey, nValue, nLeft, A3($elm$core$Dict$insertHelp, key, value, nRight));
            }
        }
    });
    var $elm$core$Dict$insert = F3(function(key, value, dict) {
        var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
        if (_v0.$ === "RBNode_elm_builtin" && _v0.a.$ === "Red") {
            var _v1 = _v0.a;
            var k = _v0.b;
            var v = _v0.c;
            var l = _v0.d;
            var r = _v0.e;
            return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
        } else {
            var x = _v0;
            return x;
        }
    });
    var $elm$browser$Debugger$Main$cachedHistory = function(model) {
        var _v0 = model.state;
        if (_v0.$ === "Running") return model.history;
        else {
            var history1 = _v0.e;
            return history1;
        }
    };
    var $elm$virtual_dom$VirtualDom$node = function(tag) {
        return _VirtualDom_node(_VirtualDom_noScript(tag));
    };
    var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
    var $elm$browser$Debugger$Main$DragEnd = {
        $: "DragEnd"
    };
    var $elm$browser$Debugger$Main$getDragStatus = function(layout) {
        if (layout.$ === "Horizontal") {
            var status = layout.a;
            return status;
        } else {
            var status = layout.a;
            return status;
        }
    };
    var $elm$browser$Debugger$Main$Drag = function(a) {
        return {
            $: "Drag",
            a: a
        };
    };
    var $elm$browser$Debugger$Main$DragInfo = F5(function(x, y, down, width, height) {
        return {
            down: down,
            height: height,
            width: width,
            x: x,
            y: y
        };
    });
    var $elm$json$Json$Decode$field = _Json_decodeField;
    var $elm$json$Json$Decode$at = F2(function(fields, decoder) {
        return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
    });
    var $elm$json$Json$Decode$float = _Json_decodeFloat;
    var $elm$browser$Debugger$Main$decodeDimension = function(field) {
        return A2($elm$json$Json$Decode$at, _List_fromArray([
            "currentTarget",
            "ownerDocument",
            "defaultView",
            field
        ]), $elm$json$Json$Decode$float);
    };
    var $elm$json$Json$Decode$int = _Json_decodeInt;
    var $elm$json$Json$Decode$map5 = _Json_map5;
    var $elm$browser$Debugger$Main$onMouseMove = A2($elm$html$Html$Events$on, "mousemove", A2($elm$json$Json$Decode$map, $elm$browser$Debugger$Main$Drag, A6($elm$json$Json$Decode$map5, $elm$browser$Debugger$Main$DragInfo, A2($elm$json$Json$Decode$field, "pageX", $elm$json$Json$Decode$float), A2($elm$json$Json$Decode$field, "pageY", $elm$json$Json$Decode$float), A2($elm$json$Json$Decode$field, "buttons", A2($elm$json$Json$Decode$map, function(v) {
        return v === 1;
    }, $elm$json$Json$Decode$int)), $elm$browser$Debugger$Main$decodeDimension("innerWidth"), $elm$browser$Debugger$Main$decodeDimension("innerHeight"))));
    var $elm$html$Html$Events$onMouseUp = function(msg) {
        return A2($elm$html$Html$Events$on, "mouseup", $elm$json$Json$Decode$succeed(msg));
    };
    var $elm$browser$Debugger$Main$toDragListeners = function(layout) {
        var _v0 = $elm$browser$Debugger$Main$getDragStatus(layout);
        if (_v0.$ === "Static") return _List_Nil;
        else return _List_fromArray([
            $elm$browser$Debugger$Main$onMouseMove,
            $elm$html$Html$Events$onMouseUp($elm$browser$Debugger$Main$DragEnd)
        ]);
    };
    var $elm$browser$Debugger$Main$toFlexDirection = function(layout) {
        if (layout.$ === "Horizontal") return "row";
        else return "column-reverse";
    };
    var $elm$browser$Debugger$Main$DragStart = {
        $: "DragStart"
    };
    var $elm$html$Html$Events$onMouseDown = function(msg) {
        return A2($elm$html$Html$Events$on, "mousedown", $elm$json$Json$Decode$succeed(msg));
    };
    var $elm$browser$Debugger$Main$toPercent = function(fraction) {
        return $elm$core$String$fromFloat(100 * fraction) + "%";
    };
    var $elm$browser$Debugger$Main$viewDragZone = function(layout) {
        if (layout.$ === "Horizontal") {
            var x = layout.b;
            return A2($elm$html$Html$div, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "position", "absolute"),
                A2($elm$html$Html$Attributes$style, "top", "0"),
                A2($elm$html$Html$Attributes$style, "left", $elm$browser$Debugger$Main$toPercent(x)),
                A2($elm$html$Html$Attributes$style, "margin-left", "-5px"),
                A2($elm$html$Html$Attributes$style, "width", "10px"),
                A2($elm$html$Html$Attributes$style, "height", "100%"),
                A2($elm$html$Html$Attributes$style, "cursor", "col-resize"),
                $elm$html$Html$Events$onMouseDown($elm$browser$Debugger$Main$DragStart)
            ]), _List_Nil);
        } else {
            var y = layout.c;
            return A2($elm$html$Html$div, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "position", "absolute"),
                A2($elm$html$Html$Attributes$style, "top", $elm$browser$Debugger$Main$toPercent(y)),
                A2($elm$html$Html$Attributes$style, "left", "0"),
                A2($elm$html$Html$Attributes$style, "margin-top", "-5px"),
                A2($elm$html$Html$Attributes$style, "width", "100%"),
                A2($elm$html$Html$Attributes$style, "height", "10px"),
                A2($elm$html$Html$Attributes$style, "cursor", "row-resize"),
                $elm$html$Html$Events$onMouseDown($elm$browser$Debugger$Main$DragStart)
            ]), _List_Nil);
        }
    };
    var $elm$browser$Debugger$Main$TweakExpandoModel = function(a) {
        return {
            $: "TweakExpandoModel",
            a: a
        };
    };
    var $elm$browser$Debugger$Main$TweakExpandoMsg = function(a) {
        return {
            $: "TweakExpandoMsg",
            a: a
        };
    };
    var $elm$browser$Debugger$Main$toExpandoPercents = function(layout) {
        if (layout.$ === "Horizontal") {
            var x = layout.b;
            return _Utils_Tuple2($elm$browser$Debugger$Main$toPercent(1 - x), "100%");
        } else {
            var y = layout.c;
            return _Utils_Tuple2("100%", $elm$browser$Debugger$Main$toPercent(y));
        }
    };
    var $elm$browser$Debugger$Main$toMouseBlocker = function(layout) {
        var _v0 = $elm$browser$Debugger$Main$getDragStatus(layout);
        if (_v0.$ === "Static") return "auto";
        else return "none";
    };
    var $elm$browser$Debugger$Expando$Field = F2(function(a, b) {
        return {
            $: "Field",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$Expando$Index = F3(function(a, b, c) {
        return {
            $: "Index",
            a: a,
            b: b,
            c: c
        };
    });
    var $elm$browser$Debugger$Expando$Key = {
        $: "Key"
    };
    var $elm$browser$Debugger$Expando$None = {
        $: "None"
    };
    var $elm$browser$Debugger$Expando$Toggle = {
        $: "Toggle"
    };
    var $elm$browser$Debugger$Expando$Value = {
        $: "Value"
    };
    var $elm$browser$Debugger$Expando$blue = A2($elm$html$Html$Attributes$style, "color", "rgb(28, 0, 207)");
    var $elm$core$Basics$composeL = F3(function(g, f, x) {
        return g(f(x));
    });
    var $elm$browser$Debugger$Expando$leftPad = function(maybeKey) {
        if (maybeKey.$ === "Nothing") return _List_Nil;
        else return _List_fromArray([
            A2($elm$html$Html$Attributes$style, "padding-left", "4ch")
        ]);
    };
    var $elm$browser$Debugger$Expando$makeArrow = function(arrow) {
        return A2($elm$html$Html$span, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "color", "#777"),
            A2($elm$html$Html$Attributes$style, "padding-left", "2ch"),
            A2($elm$html$Html$Attributes$style, "width", "2ch"),
            A2($elm$html$Html$Attributes$style, "display", "inline-block")
        ]), _List_fromArray([
            $elm$html$Html$text(arrow)
        ]));
    };
    var $elm$browser$Debugger$Expando$purple = A2($elm$html$Html$Attributes$style, "color", "rgb(136, 19, 145)");
    var $elm$browser$Debugger$Expando$lineStarter = F3(function(maybeKey, maybeIsClosed, description) {
        var arrow = function() {
            if (maybeIsClosed.$ === "Nothing") return $elm$browser$Debugger$Expando$makeArrow("");
            else {
                if (maybeIsClosed.a) return $elm$browser$Debugger$Expando$makeArrow("▸");
                else return $elm$browser$Debugger$Expando$makeArrow("▾");
            }
        }();
        if (maybeKey.$ === "Nothing") return A2($elm$core$List$cons, arrow, description);
        else {
            var key = maybeKey.a;
            return A2($elm$core$List$cons, arrow, A2($elm$core$List$cons, A2($elm$html$Html$span, _List_fromArray([
                $elm$browser$Debugger$Expando$purple
            ]), _List_fromArray([
                $elm$html$Html$text(key)
            ])), A2($elm$core$List$cons, $elm$html$Html$text(" = "), description)));
        }
    });
    var $elm$browser$Debugger$Expando$red = A2($elm$html$Html$Attributes$style, "color", "rgb(196, 26, 22)");
    var $elm$core$Tuple$second = function(_v0) {
        var y = _v0.b;
        return y;
    };
    var $elm$browser$Debugger$Expando$seqTypeToString = F2(function(n, seqType) {
        switch(seqType.$){
            case "ListSeq":
                return "List(" + ($elm$core$String$fromInt(n) + ")");
            case "SetSeq":
                return "Set(" + ($elm$core$String$fromInt(n) + ")");
            default:
                return "Array(" + ($elm$core$String$fromInt(n) + ")");
        }
    });
    var $elm$core$String$slice = _String_slice;
    var $elm$core$String$left = F2(function(n, string) {
        return n < 1 ? "" : A3($elm$core$String$slice, 0, n, string);
    });
    var $elm$core$String$right = F2(function(n, string) {
        return n < 1 ? "" : A3($elm$core$String$slice, -n, $elm$core$String$length(string), string);
    });
    var $elm$browser$Debugger$Expando$elideMiddle = function(str) {
        return $elm$core$String$length(str) <= 18 ? str : A2($elm$core$String$left, 8, str) + ("..." + A2($elm$core$String$right, 8, str));
    };
    var $elm$core$Dict$isEmpty = function(dict) {
        if (dict.$ === "RBEmpty_elm_builtin") return true;
        else return false;
    };
    var $elm$browser$Debugger$Expando$viewExtraTinyRecord = F3(function(length, starter, entries) {
        if (!entries.b) return _Utils_Tuple2(length + 1, _List_fromArray([
            $elm$html$Html$text("}")
        ]));
        else {
            var field = entries.a;
            var rest = entries.b;
            var nextLength = length + $elm$core$String$length(field) + 1;
            if (nextLength > 18) return _Utils_Tuple2(length + 2, _List_fromArray([
                $elm$html$Html$text("…}")
            ]));
            else {
                var _v1 = A3($elm$browser$Debugger$Expando$viewExtraTinyRecord, nextLength, ",", rest);
                var finalLength = _v1.a;
                var otherHtmls = _v1.b;
                return _Utils_Tuple2(finalLength, A2($elm$core$List$cons, $elm$html$Html$text(starter), A2($elm$core$List$cons, A2($elm$html$Html$span, _List_fromArray([
                    $elm$browser$Debugger$Expando$purple
                ]), _List_fromArray([
                    $elm$html$Html$text(field)
                ])), otherHtmls)));
            }
        }
    });
    var $elm$browser$Debugger$Expando$viewTinyHelp = function(str) {
        return _Utils_Tuple2($elm$core$String$length(str), _List_fromArray([
            $elm$html$Html$text(str)
        ]));
    };
    var $elm$core$Maybe$withDefault = F2(function(_default, maybe) {
        if (maybe.$ === "Just") {
            var value = maybe.a;
            return value;
        } else return _default;
    });
    var $elm$browser$Debugger$Expando$viewExtraTiny = function(value) {
        if (value.$ === "Record") {
            var record = value.b;
            return A3($elm$browser$Debugger$Expando$viewExtraTinyRecord, 0, "{", $elm$core$Dict$keys(record));
        } else return $elm$browser$Debugger$Expando$viewTiny(value);
    };
    var $elm$browser$Debugger$Expando$viewTiny = function(value) {
        switch(value.$){
            case "S":
                var stringRep = value.a;
                var str = $elm$browser$Debugger$Expando$elideMiddle(stringRep);
                return _Utils_Tuple2($elm$core$String$length(str), _List_fromArray([
                    A2($elm$html$Html$span, _List_fromArray([
                        $elm$browser$Debugger$Expando$red
                    ]), _List_fromArray([
                        $elm$html$Html$text(str)
                    ]))
                ]));
            case "Primitive":
                var stringRep = value.a;
                return _Utils_Tuple2($elm$core$String$length(stringRep), _List_fromArray([
                    A2($elm$html$Html$span, _List_fromArray([
                        $elm$browser$Debugger$Expando$blue
                    ]), _List_fromArray([
                        $elm$html$Html$text(stringRep)
                    ]))
                ]));
            case "Sequence":
                var seqType = value.a;
                var valueList = value.c;
                return $elm$browser$Debugger$Expando$viewTinyHelp(A2($elm$browser$Debugger$Expando$seqTypeToString, $elm$core$List$length(valueList), seqType));
            case "Dictionary":
                var keyValuePairs = value.b;
                return $elm$browser$Debugger$Expando$viewTinyHelp("Dict(" + ($elm$core$String$fromInt($elm$core$List$length(keyValuePairs)) + ")"));
            case "Record":
                var record = value.b;
                return $elm$browser$Debugger$Expando$viewTinyRecord(record);
            default:
                if (!value.c.b) {
                    var maybeName = value.a;
                    return $elm$browser$Debugger$Expando$viewTinyHelp(A2($elm$core$Maybe$withDefault, "Unit", maybeName));
                } else {
                    var maybeName = value.a;
                    var valueList = value.c;
                    return $elm$browser$Debugger$Expando$viewTinyHelp(function() {
                        if (maybeName.$ === "Nothing") return "Tuple(" + ($elm$core$String$fromInt($elm$core$List$length(valueList)) + ")");
                        else {
                            var name = maybeName.a;
                            return name + " …";
                        }
                    }());
                }
        }
    };
    var $elm$browser$Debugger$Expando$viewTinyRecord = function(record) {
        return $elm$core$Dict$isEmpty(record) ? _Utils_Tuple2(2, _List_fromArray([
            $elm$html$Html$text("{}")
        ])) : A3($elm$browser$Debugger$Expando$viewTinyRecordHelp, 0, "{ ", $elm$core$Dict$toList(record));
    };
    var $elm$browser$Debugger$Expando$viewTinyRecordHelp = F3(function(length, starter, entries) {
        if (!entries.b) return _Utils_Tuple2(length + 2, _List_fromArray([
            $elm$html$Html$text(" }")
        ]));
        else {
            var _v1 = entries.a;
            var field = _v1.a;
            var value = _v1.b;
            var rest = entries.b;
            var fieldLen = $elm$core$String$length(field);
            var _v2 = $elm$browser$Debugger$Expando$viewExtraTiny(value);
            var valueLen = _v2.a;
            var valueHtmls = _v2.b;
            var newLength = length + fieldLen + valueLen + 5;
            if (newLength > 60) return _Utils_Tuple2(length + 4, _List_fromArray([
                $elm$html$Html$text(", … }")
            ]));
            else {
                var _v3 = A3($elm$browser$Debugger$Expando$viewTinyRecordHelp, newLength, ", ", rest);
                var finalLength = _v3.a;
                var otherHtmls = _v3.b;
                return _Utils_Tuple2(finalLength, A2($elm$core$List$cons, $elm$html$Html$text(starter), A2($elm$core$List$cons, A2($elm$html$Html$span, _List_fromArray([
                    $elm$browser$Debugger$Expando$purple
                ]), _List_fromArray([
                    $elm$html$Html$text(field)
                ])), A2($elm$core$List$cons, $elm$html$Html$text(" = "), A2($elm$core$List$cons, A2($elm$html$Html$span, _List_Nil, valueHtmls), otherHtmls)))));
            }
        }
    });
    var $elm$browser$Debugger$Expando$view = F2(function(maybeKey, expando) {
        switch(expando.$){
            case "S":
                var stringRep = expando.a;
                return A2($elm$html$Html$div, $elm$browser$Debugger$Expando$leftPad(maybeKey), A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, $elm$core$Maybe$Nothing, _List_fromArray([
                    A2($elm$html$Html$span, _List_fromArray([
                        $elm$browser$Debugger$Expando$red
                    ]), _List_fromArray([
                        $elm$html$Html$text(stringRep)
                    ]))
                ])));
            case "Primitive":
                var stringRep = expando.a;
                return A2($elm$html$Html$div, $elm$browser$Debugger$Expando$leftPad(maybeKey), A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, $elm$core$Maybe$Nothing, _List_fromArray([
                    A2($elm$html$Html$span, _List_fromArray([
                        $elm$browser$Debugger$Expando$blue
                    ]), _List_fromArray([
                        $elm$html$Html$text(stringRep)
                    ]))
                ])));
            case "Sequence":
                var seqType = expando.a;
                var isClosed = expando.b;
                var valueList = expando.c;
                return A4($elm$browser$Debugger$Expando$viewSequence, maybeKey, seqType, isClosed, valueList);
            case "Dictionary":
                var isClosed = expando.a;
                var keyValuePairs = expando.b;
                return A3($elm$browser$Debugger$Expando$viewDictionary, maybeKey, isClosed, keyValuePairs);
            case "Record":
                var isClosed = expando.a;
                var valueDict = expando.b;
                return A3($elm$browser$Debugger$Expando$viewRecord, maybeKey, isClosed, valueDict);
            default:
                var maybeName = expando.a;
                var isClosed = expando.b;
                var valueList = expando.c;
                return A4($elm$browser$Debugger$Expando$viewConstructor, maybeKey, maybeName, isClosed, valueList);
        }
    });
    var $elm$browser$Debugger$Expando$viewConstructor = F4(function(maybeKey, maybeName, isClosed, valueList) {
        var tinyArgs = A2($elm$core$List$map, A2($elm$core$Basics$composeL, $elm$core$Tuple$second, $elm$browser$Debugger$Expando$viewExtraTiny), valueList);
        var description = function() {
            var _v7 = _Utils_Tuple2(maybeName, tinyArgs);
            if (_v7.a.$ === "Nothing") {
                if (!_v7.b.b) {
                    var _v8 = _v7.a;
                    return _List_fromArray([
                        $elm$html$Html$text("()")
                    ]);
                } else {
                    var _v9 = _v7.a;
                    var _v10 = _v7.b;
                    var x = _v10.a;
                    var xs = _v10.b;
                    return A2($elm$core$List$cons, $elm$html$Html$text("( "), A2($elm$core$List$cons, A2($elm$html$Html$span, _List_Nil, x), A3($elm$core$List$foldr, F2(function(args, rest) {
                        return A2($elm$core$List$cons, $elm$html$Html$text(", "), A2($elm$core$List$cons, A2($elm$html$Html$span, _List_Nil, args), rest));
                    }), _List_fromArray([
                        $elm$html$Html$text(" )")
                    ]), xs)));
                }
            } else if (!_v7.b.b) {
                var name = _v7.a.a;
                return _List_fromArray([
                    $elm$html$Html$text(name)
                ]);
            } else {
                var name = _v7.a.a;
                var _v11 = _v7.b;
                var x = _v11.a;
                var xs = _v11.b;
                return A2($elm$core$List$cons, $elm$html$Html$text(name + " "), A2($elm$core$List$cons, A2($elm$html$Html$span, _List_Nil, x), A3($elm$core$List$foldr, F2(function(args, rest) {
                    return A2($elm$core$List$cons, $elm$html$Html$text(" "), A2($elm$core$List$cons, A2($elm$html$Html$span, _List_Nil, args), rest));
                }), _List_Nil, xs)));
            }
        }();
        var _v4 = function() {
            if (!valueList.b) return _Utils_Tuple2($elm$core$Maybe$Nothing, A2($elm$html$Html$div, _List_Nil, _List_Nil));
            else {
                if (!valueList.b.b) {
                    var entry = valueList.a;
                    switch(entry.$){
                        case "S":
                            return _Utils_Tuple2($elm$core$Maybe$Nothing, A2($elm$html$Html$div, _List_Nil, _List_Nil));
                        case "Primitive":
                            return _Utils_Tuple2($elm$core$Maybe$Nothing, A2($elm$html$Html$div, _List_Nil, _List_Nil));
                        case "Sequence":
                            var subValueList = entry.c;
                            return _Utils_Tuple2($elm$core$Maybe$Just(isClosed), isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0), $elm$browser$Debugger$Expando$viewSequenceOpen(subValueList)));
                        case "Dictionary":
                            var keyValuePairs = entry.b;
                            return _Utils_Tuple2($elm$core$Maybe$Just(isClosed), isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0), $elm$browser$Debugger$Expando$viewDictionaryOpen(keyValuePairs)));
                        case "Record":
                            var record = entry.b;
                            return _Utils_Tuple2($elm$core$Maybe$Just(isClosed), isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0), $elm$browser$Debugger$Expando$viewRecordOpen(record)));
                        default:
                            var subValueList = entry.c;
                            return _Utils_Tuple2($elm$core$Maybe$Just(isClosed), isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0), $elm$browser$Debugger$Expando$viewConstructorOpen(subValueList)));
                    }
                } else return _Utils_Tuple2($elm$core$Maybe$Just(isClosed), isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : $elm$browser$Debugger$Expando$viewConstructorOpen(valueList));
            }
        }();
        var maybeIsClosed = _v4.a;
        var openHtml = _v4.b;
        return A2($elm$html$Html$div, $elm$browser$Debugger$Expando$leftPad(maybeKey), _List_fromArray([
            A2($elm$html$Html$div, _List_fromArray([
                $elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
            ]), A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, maybeIsClosed, description)),
            openHtml
        ]));
    });
    var $elm$browser$Debugger$Expando$viewConstructorEntry = F2(function(index, value) {
        return A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, index), A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Just($elm$core$String$fromInt(index)), value));
    });
    var $elm$browser$Debugger$Expando$viewConstructorOpen = function(valueList) {
        return A2($elm$html$Html$div, _List_Nil, A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewConstructorEntry, valueList));
    };
    var $elm$browser$Debugger$Expando$viewDictionary = F3(function(maybeKey, isClosed, keyValuePairs) {
        var starter = "Dict(" + ($elm$core$String$fromInt($elm$core$List$length(keyValuePairs)) + ")");
        return A2($elm$html$Html$div, $elm$browser$Debugger$Expando$leftPad(maybeKey), _List_fromArray([
            A2($elm$html$Html$div, _List_fromArray([
                $elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
            ]), A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, $elm$core$Maybe$Just(isClosed), _List_fromArray([
                $elm$html$Html$text(starter)
            ]))),
            isClosed ? $elm$html$Html$text("") : $elm$browser$Debugger$Expando$viewDictionaryOpen(keyValuePairs)
        ]));
    });
    var $elm$browser$Debugger$Expando$viewDictionaryEntry = F2(function(index, _v2) {
        var key = _v2.a;
        var value = _v2.b;
        switch(key.$){
            case "S":
                var stringRep = key.a;
                return A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index), A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Just(stringRep), value));
            case "Primitive":
                var stringRep = key.a;
                return A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index), A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Just(stringRep), value));
            default:
                return A2($elm$html$Html$div, _List_Nil, _List_fromArray([
                    A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Key, index), A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Just("key"), key)),
                    A2($elm$html$Html$map, A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index), A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Just("value"), value))
                ]));
        }
    });
    var $elm$browser$Debugger$Expando$viewDictionaryOpen = function(keyValuePairs) {
        return A2($elm$html$Html$div, _List_Nil, A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewDictionaryEntry, keyValuePairs));
    };
    var $elm$browser$Debugger$Expando$viewRecord = F3(function(maybeKey, isClosed, record) {
        var _v1 = isClosed ? _Utils_Tuple3($elm$browser$Debugger$Expando$viewTinyRecord(record).b, $elm$html$Html$text(""), $elm$html$Html$text("")) : _Utils_Tuple3(_List_fromArray([
            $elm$html$Html$text("{")
        ]), $elm$browser$Debugger$Expando$viewRecordOpen(record), A2($elm$html$Html$div, $elm$browser$Debugger$Expando$leftPad($elm$core$Maybe$Just(_Utils_Tuple0)), _List_fromArray([
            $elm$html$Html$text("}")
        ])));
        var start = _v1.a;
        var middle = _v1.b;
        var end = _v1.c;
        return A2($elm$html$Html$div, $elm$browser$Debugger$Expando$leftPad(maybeKey), _List_fromArray([
            A2($elm$html$Html$div, _List_fromArray([
                $elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
            ]), A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, $elm$core$Maybe$Just(isClosed), start)),
            middle,
            end
        ]));
    });
    var $elm$browser$Debugger$Expando$viewRecordEntry = function(_v0) {
        var field = _v0.a;
        var value = _v0.b;
        return A2($elm$html$Html$map, $elm$browser$Debugger$Expando$Field(field), A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Just(field), value));
    };
    var $elm$browser$Debugger$Expando$viewRecordOpen = function(record) {
        return A2($elm$html$Html$div, _List_Nil, A2($elm$core$List$map, $elm$browser$Debugger$Expando$viewRecordEntry, $elm$core$Dict$toList(record)));
    };
    var $elm$browser$Debugger$Expando$viewSequence = F4(function(maybeKey, seqType, isClosed, valueList) {
        var starter = A2($elm$browser$Debugger$Expando$seqTypeToString, $elm$core$List$length(valueList), seqType);
        return A2($elm$html$Html$div, $elm$browser$Debugger$Expando$leftPad(maybeKey), _List_fromArray([
            A2($elm$html$Html$div, _List_fromArray([
                $elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
            ]), A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, $elm$core$Maybe$Just(isClosed), _List_fromArray([
                $elm$html$Html$text(starter)
            ]))),
            isClosed ? $elm$html$Html$text("") : $elm$browser$Debugger$Expando$viewSequenceOpen(valueList)
        ]));
    });
    var $elm$browser$Debugger$Expando$viewSequenceOpen = function(values) {
        return A2($elm$html$Html$div, _List_Nil, A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewConstructorEntry, values));
    };
    var $elm$browser$Debugger$Main$viewExpando = F3(function(expandoMsg, expandoModel, layout) {
        var block = $elm$browser$Debugger$Main$toMouseBlocker(layout);
        var _v0 = $elm$browser$Debugger$Main$toExpandoPercents(layout);
        var w = _v0.a;
        var h = _v0.b;
        return A2($elm$html$Html$div, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "display", "block"),
            A2($elm$html$Html$Attributes$style, "width", "calc(" + (w + " - 4em)")),
            A2($elm$html$Html$Attributes$style, "height", "calc(" + (h + " - 4em)")),
            A2($elm$html$Html$Attributes$style, "padding", "2em"),
            A2($elm$html$Html$Attributes$style, "margin", "0"),
            A2($elm$html$Html$Attributes$style, "overflow", "auto"),
            A2($elm$html$Html$Attributes$style, "pointer-events", block),
            A2($elm$html$Html$Attributes$style, "-webkit-user-select", block),
            A2($elm$html$Html$Attributes$style, "-moz-user-select", block),
            A2($elm$html$Html$Attributes$style, "-ms-user-select", block),
            A2($elm$html$Html$Attributes$style, "user-select", block)
        ]), _List_fromArray([
            A2($elm$html$Html$div, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "color", "#ccc"),
                A2($elm$html$Html$Attributes$style, "padding", "0 0 1em 0")
            ]), _List_fromArray([
                $elm$html$Html$text("-- MESSAGE")
            ])),
            A2($elm$html$Html$map, $elm$browser$Debugger$Main$TweakExpandoMsg, A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Nothing, expandoMsg)),
            A2($elm$html$Html$div, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "color", "#ccc"),
                A2($elm$html$Html$Attributes$style, "padding", "1em 0")
            ]), _List_fromArray([
                $elm$html$Html$text("-- MODEL")
            ])),
            A2($elm$html$Html$map, $elm$browser$Debugger$Main$TweakExpandoModel, A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Nothing, expandoModel))
        ]));
    });
    var $elm$browser$Debugger$Main$Jump = function(a) {
        return {
            $: "Jump",
            a: a
        };
    };
    var $elm$virtual_dom$VirtualDom$lazy = _VirtualDom_lazy;
    var $elm$html$Html$Lazy$lazy = $elm$virtual_dom$VirtualDom$lazy;
    var $elm$browser$Debugger$Main$toHistoryPercents = function(layout) {
        if (layout.$ === "Horizontal") {
            var x = layout.b;
            return _Utils_Tuple2($elm$browser$Debugger$Main$toPercent(x), "100%");
        } else {
            var y = layout.c;
            return _Utils_Tuple2("100%", $elm$browser$Debugger$Main$toPercent(1 - y));
        }
    };
    var $elm$virtual_dom$VirtualDom$lazy3 = _VirtualDom_lazy3;
    var $elm$html$Html$Lazy$lazy3 = $elm$virtual_dom$VirtualDom$lazy3;
    var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty("className");
    var $elm$browser$Debugger$History$idForMessageIndex = function(index) {
        return "msg-" + $elm$core$String$fromInt(index);
    };
    var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty("title");
    var $elm$browser$Debugger$History$viewMessage = F3(function(currentIndex, index, msg) {
        var messageName = _Debugger_messageToString(msg);
        var className = _Utils_eq(currentIndex, index) ? "elm-debugger-entry elm-debugger-entry-selected" : "elm-debugger-entry";
        return A2($elm$html$Html$div, _List_fromArray([
            $elm$html$Html$Attributes$id($elm$browser$Debugger$History$idForMessageIndex(index)),
            $elm$html$Html$Attributes$class(className),
            $elm$html$Html$Events$onClick(index)
        ]), _List_fromArray([
            A2($elm$html$Html$span, _List_fromArray([
                $elm$html$Html$Attributes$title(messageName),
                $elm$html$Html$Attributes$class("elm-debugger-entry-content")
            ]), _List_fromArray([
                $elm$html$Html$text(messageName)
            ])),
            A2($elm$html$Html$span, _List_fromArray([
                $elm$html$Html$Attributes$class("elm-debugger-entry-index")
            ]), _List_fromArray([
                $elm$html$Html$text($elm$core$String$fromInt(index))
            ]))
        ]));
    });
    var $elm$browser$Debugger$History$consMsg = F3(function(currentIndex, msg, _v0) {
        var index = _v0.a;
        var rest = _v0.b;
        return _Utils_Tuple2(index + 1, A2($elm$core$List$cons, _Utils_Tuple2($elm$core$String$fromInt(index), A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewMessage, currentIndex, index, msg)), rest));
    });
    var $elm$core$Array$length = function(_v0) {
        var len = _v0.a;
        return len;
    };
    var $elm$core$Basics$neq = _Utils_notEqual;
    var $elm$virtual_dom$VirtualDom$keyedNode = function(tag) {
        return _VirtualDom_keyedNode(_VirtualDom_noScript(tag));
    };
    var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
    var $elm$browser$Debugger$History$maxSnapshotSize = 31;
    var $elm$browser$Debugger$History$showMoreButton = function(numMessages) {
        var nextIndex = numMessages - 1 - $elm$browser$Debugger$History$maxSnapshotSize * 2;
        var labelText = "View more messages";
        return A2($elm$html$Html$div, _List_fromArray([
            $elm$html$Html$Attributes$class("elm-debugger-entry"),
            $elm$html$Html$Events$onClick(nextIndex)
        ]), _List_fromArray([
            A2($elm$html$Html$span, _List_fromArray([
                $elm$html$Html$Attributes$title(labelText),
                $elm$html$Html$Attributes$class("elm-debugger-entry-content")
            ]), _List_fromArray([
                $elm$html$Html$text(labelText)
            ])),
            A2($elm$html$Html$span, _List_fromArray([
                $elm$html$Html$Attributes$class("elm-debugger-entry-index")
            ]), _List_Nil)
        ]));
    };
    var $elm$browser$Debugger$History$styles = A3($elm$html$Html$node, "style", _List_Nil, _List_fromArray([
        $elm$html$Html$text("\n\n.elm-debugger-entry {\n  cursor: pointer;\n  width: 100%;\n  box-sizing: border-box;\n  padding: 8px;\n}\n\n.elm-debugger-entry:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.elm-debugger-entry-selected, .elm-debugger-entry-selected:hover {\n  background-color: rgb(10, 10, 10);\n}\n\n.elm-debugger-entry-content {\n  width: calc(100% - 40px);\n  padding: 0 5px;\n  box-sizing: border-box;\n  text-overflow: ellipsis;\n  white-space: nowrap;\n  overflow: hidden;\n  display: inline-block;\n}\n\n.elm-debugger-entry-index {\n  color: #666;\n  width: 40px;\n  text-align: right;\n  display: block;\n  float: right;\n}\n\n")
    ]));
    var $elm$core$Basics$ge = _Utils_ge;
    var $elm$browser$Debugger$History$viewSnapshot = F3(function(selectedIndex, index, _v0) {
        var messages = _v0.messages;
        return A3($elm$html$Html$Keyed$node, "div", _List_Nil, A3($elm$core$Array$foldr, $elm$browser$Debugger$History$consMsg(selectedIndex), _Utils_Tuple2(index, _List_Nil), messages).b);
    });
    var $elm$browser$Debugger$History$consSnapshot = F3(function(selectedIndex, snapshot, _v0) {
        var index = _v0.a;
        var rest = _v0.b;
        var nextIndex = index + $elm$core$Array$length(snapshot.messages);
        var selectedIndexHelp = _Utils_cmp(nextIndex, selectedIndex) > 0 && _Utils_cmp(selectedIndex, index) > -1 ? selectedIndex : -1;
        return _Utils_Tuple2(nextIndex, A2($elm$core$List$cons, A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewSnapshot, selectedIndexHelp, index, snapshot), rest));
    });
    var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
    var $elm$core$Array$foldl = F3(function(func, baseCase, _v0) {
        var tree = _v0.c;
        var tail = _v0.d;
        var helper = F2(function(node, acc) {
            if (node.$ === "SubTree") {
                var subTree = node.a;
                return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
            } else {
                var values = node.a;
                return A3($elm$core$Elm$JsArray$foldl, func, acc, values);
            }
        });
        return A3($elm$core$Elm$JsArray$foldl, func, A3($elm$core$Elm$JsArray$foldl, helper, baseCase, tree), tail);
    });
    var $elm$browser$Debugger$History$viewAllSnapshots = F3(function(selectedIndex, startIndex, snapshots) {
        return A2($elm$html$Html$div, _List_Nil, A3($elm$core$Array$foldl, $elm$browser$Debugger$History$consSnapshot(selectedIndex), _Utils_Tuple2(startIndex, _List_Nil), snapshots).b);
    });
    var $elm$core$Array$fromListHelp = F3(function(list, nodeList, nodeListSize) {
        fromListHelp: while(true){
            var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
            var jsArray = _v0.a;
            var remainingItems = _v0.b;
            if (_Utils_cmp($elm$core$Elm$JsArray$length(jsArray), $elm$core$Array$branchFactor) < 0) return A2($elm$core$Array$builderToArray, true, {
                nodeList: nodeList,
                nodeListSize: nodeListSize,
                tail: jsArray
            });
            else {
                var $temp$list = remainingItems, $temp$nodeList = A2($elm$core$List$cons, $elm$core$Array$Leaf(jsArray), nodeList), $temp$nodeListSize = nodeListSize + 1;
                list = $temp$list;
                nodeList = $temp$nodeList;
                nodeListSize = $temp$nodeListSize;
                continue fromListHelp;
            }
        }
    });
    var $elm$core$Array$fromList = function(list) {
        if (!list.b) return $elm$core$Array$empty;
        else return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
    };
    var $elm$core$Bitwise$and = _Bitwise_and;
    var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
    var $elm$core$Array$bitMask = 4294967295 >>> 32 - $elm$core$Array$shiftStep;
    var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
    var $elm$core$Array$getHelp = F3(function(shift, index, tree) {
        getHelp: while(true){
            var pos = $elm$core$Array$bitMask & index >>> shift;
            var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
            if (_v0.$ === "SubTree") {
                var subTree = _v0.a;
                var $temp$shift = shift - $elm$core$Array$shiftStep, $temp$index = index, $temp$tree = subTree;
                shift = $temp$shift;
                index = $temp$index;
                tree = $temp$tree;
                continue getHelp;
            } else {
                var values = _v0.a;
                return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
            }
        }
    });
    var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
    var $elm$core$Array$tailIndex = function(len) {
        return len >>> 5 << 5;
    };
    var $elm$core$Array$get = F2(function(index, _v0) {
        var len = _v0.a;
        var startShift = _v0.b;
        var tree = _v0.c;
        var tail = _v0.d;
        return index < 0 || _Utils_cmp(index, len) > -1 ? $elm$core$Maybe$Nothing : _Utils_cmp(index, $elm$core$Array$tailIndex(len)) > -1 ? $elm$core$Maybe$Just(A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(A3($elm$core$Array$getHelp, startShift, index, tree));
    });
    var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
    var $elm$core$Elm$JsArray$slice = _JsArray_slice;
    var $elm$core$Array$appendHelpBuilder = F2(function(tail, builder) {
        var tailLen = $elm$core$Elm$JsArray$length(tail);
        var notAppended = $elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail) - tailLen;
        var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
        return notAppended < 0 ? {
            nodeList: A2($elm$core$List$cons, $elm$core$Array$Leaf(appended), builder.nodeList),
            nodeListSize: builder.nodeListSize + 1,
            tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
        } : !notAppended ? {
            nodeList: A2($elm$core$List$cons, $elm$core$Array$Leaf(appended), builder.nodeList),
            nodeListSize: builder.nodeListSize + 1,
            tail: $elm$core$Elm$JsArray$empty
        } : {
            nodeList: builder.nodeList,
            nodeListSize: builder.nodeListSize,
            tail: appended
        };
    });
    var $elm$core$List$drop = F2(function(n, list) {
        drop: while(true){
            if (n <= 0) return list;
            else {
                if (!list.b) return list;
                else {
                    var x = list.a;
                    var xs = list.b;
                    var $temp$n = n - 1, $temp$list = xs;
                    n = $temp$n;
                    list = $temp$list;
                    continue drop;
                }
            }
        }
    });
    var $elm$core$Array$sliceLeft = F2(function(from, array) {
        var len = array.a;
        var tree = array.c;
        var tail = array.d;
        if (!from) return array;
        else {
            if (_Utils_cmp(from, $elm$core$Array$tailIndex(len)) > -1) return A4($elm$core$Array$Array_elm_builtin, len - from, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, A3($elm$core$Elm$JsArray$slice, from - $elm$core$Array$tailIndex(len), $elm$core$Elm$JsArray$length(tail), tail));
            else {
                var skipNodes = from / $elm$core$Array$branchFactor | 0;
                var helper = F2(function(node, acc) {
                    if (node.$ === "SubTree") {
                        var subTree = node.a;
                        return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
                    } else {
                        var leaf = node.a;
                        return A2($elm$core$List$cons, leaf, acc);
                    }
                });
                var leafNodes = A3($elm$core$Elm$JsArray$foldr, helper, _List_fromArray([
                    tail
                ]), tree);
                var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
                if (!nodesToInsert.b) return $elm$core$Array$empty;
                else {
                    var head = nodesToInsert.a;
                    var rest = nodesToInsert.b;
                    var firstSlice = from - skipNodes * $elm$core$Array$branchFactor;
                    var initialBuilder = {
                        nodeList: _List_Nil,
                        nodeListSize: 0,
                        tail: A3($elm$core$Elm$JsArray$slice, firstSlice, $elm$core$Elm$JsArray$length(head), head)
                    };
                    return A2($elm$core$Array$builderToArray, true, A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
                }
            }
        }
    });
    var $elm$core$Array$fetchNewTail = F4(function(shift, end, treeEnd, tree) {
        fetchNewTail: while(true){
            var pos = $elm$core$Array$bitMask & treeEnd >>> shift;
            var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
            if (_v0.$ === "SubTree") {
                var sub = _v0.a;
                var $temp$shift = shift - $elm$core$Array$shiftStep, $temp$end = end, $temp$treeEnd = treeEnd, $temp$tree = sub;
                shift = $temp$shift;
                end = $temp$end;
                treeEnd = $temp$treeEnd;
                tree = $temp$tree;
                continue fetchNewTail;
            } else {
                var values = _v0.a;
                return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
            }
        }
    });
    var $elm$core$Array$hoistTree = F3(function(oldShift, newShift, tree) {
        hoistTree: while(true){
            if (_Utils_cmp(oldShift, newShift) < 1 || !$elm$core$Elm$JsArray$length(tree)) return tree;
            else {
                var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
                if (_v0.$ === "SubTree") {
                    var sub = _v0.a;
                    var $temp$oldShift = oldShift - $elm$core$Array$shiftStep, $temp$newShift = newShift, $temp$tree = sub;
                    oldShift = $temp$oldShift;
                    newShift = $temp$newShift;
                    tree = $temp$tree;
                    continue hoistTree;
                } else return tree;
            }
        }
    });
    var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
    var $elm$core$Array$sliceTree = F3(function(shift, endIdx, tree) {
        var lastPos = $elm$core$Array$bitMask & endIdx >>> shift;
        var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
        if (_v0.$ === "SubTree") {
            var sub = _v0.a;
            var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
            return !$elm$core$Elm$JsArray$length(newSub) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3($elm$core$Elm$JsArray$unsafeSet, lastPos, $elm$core$Array$SubTree(newSub), A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
        } else return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
    });
    var $elm$core$Array$sliceRight = F2(function(end, array) {
        var len = array.a;
        var startShift = array.b;
        var tree = array.c;
        var tail = array.d;
        if (_Utils_eq(end, len)) return array;
        else {
            if (_Utils_cmp(end, $elm$core$Array$tailIndex(len)) > -1) return A4($elm$core$Array$Array_elm_builtin, end, startShift, tree, A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
            else {
                var endIdx = $elm$core$Array$tailIndex(end);
                var depth = $elm$core$Basics$floor(A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, A2($elm$core$Basics$max, 1, endIdx - 1)));
                var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
                return A4($elm$core$Array$Array_elm_builtin, end, newShift, A3($elm$core$Array$hoistTree, startShift, newShift, A3($elm$core$Array$sliceTree, startShift, endIdx, tree)), A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
            }
        }
    });
    var $elm$core$Array$translateIndex = F2(function(index, _v0) {
        var len = _v0.a;
        var posIndex = index < 0 ? len + index : index;
        return posIndex < 0 ? 0 : _Utils_cmp(posIndex, len) > 0 ? len : posIndex;
    });
    var $elm$core$Array$slice = F3(function(from, to, array) {
        var correctTo = A2($elm$core$Array$translateIndex, to, array);
        var correctFrom = A2($elm$core$Array$translateIndex, from, array);
        return _Utils_cmp(correctFrom, correctTo) > 0 ? $elm$core$Array$empty : A2($elm$core$Array$sliceLeft, correctFrom, A2($elm$core$Array$sliceRight, correctTo, array));
    });
    var $elm$browser$Debugger$History$viewRecentSnapshots = F3(function(selectedIndex, recentMessagesNum, snapshots) {
        var messagesToFill = $elm$browser$Debugger$History$maxSnapshotSize - recentMessagesNum;
        var arrayLength = $elm$core$Array$length(snapshots);
        var snapshotsToRender = function() {
            var _v0 = _Utils_Tuple2(A2($elm$core$Array$get, arrayLength - 2, snapshots), A2($elm$core$Array$get, arrayLength - 1, snapshots));
            if (_v0.a.$ === "Just" && _v0.b.$ === "Just") {
                var fillerSnapshot = _v0.a.a;
                var recentSnapshot = _v0.b.a;
                return $elm$core$Array$fromList(_List_fromArray([
                    {
                        messages: A3($elm$core$Array$slice, 0, messagesToFill, fillerSnapshot.messages),
                        model: fillerSnapshot.model
                    },
                    recentSnapshot
                ]));
            } else return snapshots;
        }();
        var startingIndex = arrayLength * $elm$browser$Debugger$History$maxSnapshotSize - $elm$browser$Debugger$History$maxSnapshotSize - messagesToFill;
        return A3($elm$browser$Debugger$History$viewAllSnapshots, selectedIndex, startingIndex, snapshotsToRender);
    });
    var $elm$browser$Debugger$History$view = F2(function(maybeIndex, _v0) {
        var snapshots = _v0.snapshots;
        var recent = _v0.recent;
        var numMessages = _v0.numMessages;
        var recentMessageStartIndex = numMessages - recent.numMessages;
        var index = A2($elm$core$Maybe$withDefault, -1, maybeIndex);
        var newStuff = A3($elm$html$Html$Keyed$node, "div", _List_Nil, A3($elm$core$List$foldr, $elm$browser$Debugger$History$consMsg(index), _Utils_Tuple2(recentMessageStartIndex, _List_Nil), recent.messages).b);
        var onlyRenderRecentMessages = !_Utils_eq(index, -1) || $elm$core$Array$length(snapshots) < 2;
        var oldStuff = onlyRenderRecentMessages ? A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewAllSnapshots, index, 0, snapshots) : A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewRecentSnapshots, index, recent.numMessages, snapshots);
        return A2($elm$html$Html$div, _List_fromArray([
            $elm$html$Html$Attributes$id("elm-debugger-sidebar"),
            A2($elm$html$Html$Attributes$style, "width", "100%"),
            A2($elm$html$Html$Attributes$style, "overflow-y", "auto"),
            A2($elm$html$Html$Attributes$style, "height", "calc(100% - 72px)")
        ]), A2($elm$core$List$cons, $elm$browser$Debugger$History$styles, A2($elm$core$List$cons, newStuff, A2($elm$core$List$cons, oldStuff, onlyRenderRecentMessages ? _List_Nil : _List_fromArray([
            $elm$browser$Debugger$History$showMoreButton(numMessages)
        ])))));
    });
    var $elm$browser$Debugger$Main$SwapLayout = {
        $: "SwapLayout"
    };
    var $elm$browser$Debugger$Main$toHistoryIcon = function(layout) {
        if (layout.$ === "Horizontal") return "M13 1a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M13 3h-10a1 1 0 0 0-1 1v5h12v-5a1 1 0 0 0-1-1z M14 10h-12v2a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1z";
        else return "M0 4a3 3 0 0 1 3-3h10a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3z M2 4v8a1 1 0 0 0 1 1h2v-10h-2a1 1 0 0 0-1 1z M6 3v10h7a1 1 0 0 0 1-1v-8a1 1 0 0 0-1-1z";
    };
    var $elm$browser$Debugger$Main$icon = function(path) {
        return A4($elm$virtual_dom$VirtualDom$nodeNS, "http://www.w3.org/2000/svg", "svg", _List_fromArray([
            A2($elm$virtual_dom$VirtualDom$attribute, "viewBox", "0 0 16 16"),
            A2($elm$virtual_dom$VirtualDom$attribute, "xmlns", "http://www.w3.org/2000/svg"),
            A2($elm$virtual_dom$VirtualDom$attribute, "fill", "currentColor"),
            A2($elm$virtual_dom$VirtualDom$attribute, "width", "16px"),
            A2($elm$virtual_dom$VirtualDom$attribute, "height", "16px")
        ]), _List_fromArray([
            A4($elm$virtual_dom$VirtualDom$nodeNS, "http://www.w3.org/2000/svg", "path", _List_fromArray([
                A2($elm$virtual_dom$VirtualDom$attribute, "d", path)
            ]), _List_Nil)
        ]));
    };
    var $elm$browser$Debugger$Main$viewHistoryButton = F3(function(label, msg, path) {
        return A2($elm$html$Html$button, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "display", "flex"),
            A2($elm$html$Html$Attributes$style, "flex-direction", "row"),
            A2($elm$html$Html$Attributes$style, "align-items", "center"),
            A2($elm$html$Html$Attributes$style, "background", "none"),
            A2($elm$html$Html$Attributes$style, "border", "none"),
            A2($elm$html$Html$Attributes$style, "color", "inherit"),
            A2($elm$html$Html$Attributes$style, "cursor", "pointer"),
            $elm$html$Html$Events$onClick(msg)
        ]), _List_fromArray([
            $elm$browser$Debugger$Main$icon(path),
            A2($elm$html$Html$span, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "padding-left", "6px")
            ]), _List_fromArray([
                $elm$html$Html$text(label)
            ]))
        ]));
    });
    var $elm$browser$Debugger$Main$viewHistoryOptions = function(layout) {
        return A2($elm$html$Html$div, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "width", "100%"),
            A2($elm$html$Html$Attributes$style, "height", "36px"),
            A2($elm$html$Html$Attributes$style, "display", "flex"),
            A2($elm$html$Html$Attributes$style, "flex-direction", "row"),
            A2($elm$html$Html$Attributes$style, "align-items", "center"),
            A2($elm$html$Html$Attributes$style, "justify-content", "space-between"),
            A2($elm$html$Html$Attributes$style, "background-color", "rgb(50, 50, 50)")
        ]), _List_fromArray([
            A3($elm$browser$Debugger$Main$viewHistoryButton, "Swap Layout", $elm$browser$Debugger$Main$SwapLayout, $elm$browser$Debugger$Main$toHistoryIcon(layout)),
            A2($elm$html$Html$div, _List_fromArray([
                A2($elm$html$Html$Attributes$style, "display", "flex"),
                A2($elm$html$Html$Attributes$style, "flex-direction", "row"),
                A2($elm$html$Html$Attributes$style, "align-items", "center"),
                A2($elm$html$Html$Attributes$style, "justify-content", "space-between")
            ]), _List_fromArray([
                A3($elm$browser$Debugger$Main$viewHistoryButton, "Import", $elm$browser$Debugger$Main$Import, "M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M10 2a1 1 0 0 0 -2 0v6a1 1 0 0 0 1 1h6a1 1 0 0 0 0-2h-3.586l4.293-4.293a1 1 0 0 0-1.414-1.414l-4.293 4.293z"),
                A3($elm$browser$Debugger$Main$viewHistoryButton, "Export", $elm$browser$Debugger$Main$Export, "M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1 a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M9 3a1 1 0 1 1 0-2h6a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-3.586l-5.293 5.293 a1 1 0 0 1-1.414-1.414l5.293 -5.293z")
            ]))
        ]));
    };
    var $elm$browser$Debugger$Main$SliderJump = function(a) {
        return {
            $: "SliderJump",
            a: a
        };
    };
    var $elm$core$Basics$composeR = F3(function(f, g, x) {
        return g(f(x));
    });
    var $elm$html$Html$input = _VirtualDom_node("input");
    var $elm$browser$Debugger$Main$isPlaying = function(maybeIndex) {
        if (maybeIndex.$ === "Nothing") return true;
        else return false;
    };
    var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty("max");
    var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty("min");
    var $elm$html$Html$Events$alwaysStop = function(x) {
        return _Utils_Tuple2(x, true);
    };
    var $elm$virtual_dom$VirtualDom$MayStopPropagation = function(a) {
        return {
            $: "MayStopPropagation",
            a: a
        };
    };
    var $elm$html$Html$Events$stopPropagationOn = F2(function(event, decoder) {
        return A2($elm$virtual_dom$VirtualDom$on, event, $elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
    });
    var $elm$json$Json$Decode$string = _Json_decodeString;
    var $elm$html$Html$Events$targetValue = A2($elm$json$Json$Decode$at, _List_fromArray([
        "target",
        "value"
    ]), $elm$json$Json$Decode$string);
    var $elm$html$Html$Events$onInput = function(tagger) {
        return A2($elm$html$Html$Events$stopPropagationOn, "input", A2($elm$json$Json$Decode$map, $elm$html$Html$Events$alwaysStop, A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
    };
    var $elm$core$String$toInt = _String_toInt;
    var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty("type");
    var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty("value");
    var $elm$browser$Debugger$Main$viewPlayButton = function(playing) {
        return A2($elm$html$Html$button, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "background", "#1293D8"),
            A2($elm$html$Html$Attributes$style, "border", "none"),
            A2($elm$html$Html$Attributes$style, "color", "white"),
            A2($elm$html$Html$Attributes$style, "cursor", "pointer"),
            A2($elm$html$Html$Attributes$style, "width", "36px"),
            A2($elm$html$Html$Attributes$style, "height", "36px"),
            $elm$html$Html$Events$onClick($elm$browser$Debugger$Main$Resume)
        ]), _List_fromArray([
            playing ? $elm$browser$Debugger$Main$icon("M2 2h4v12h-4v-12z M10 2h4v12h-4v-12z") : $elm$browser$Debugger$Main$icon("M2 2l12 7l-12 7z")
        ]));
    };
    var $elm$browser$Debugger$Main$viewHistorySlider = F2(function(history1, maybeIndex) {
        var lastIndex = $elm$browser$Debugger$History$size(history1) - 1;
        var selectedIndex = A2($elm$core$Maybe$withDefault, lastIndex, maybeIndex);
        return A2($elm$html$Html$div, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "display", "flex"),
            A2($elm$html$Html$Attributes$style, "flex-direction", "row"),
            A2($elm$html$Html$Attributes$style, "align-items", "center"),
            A2($elm$html$Html$Attributes$style, "width", "100%"),
            A2($elm$html$Html$Attributes$style, "height", "36px"),
            A2($elm$html$Html$Attributes$style, "background-color", "rgb(50, 50, 50)")
        ]), _List_fromArray([
            A2($elm$html$Html$Lazy$lazy, $elm$browser$Debugger$Main$viewPlayButton, $elm$browser$Debugger$Main$isPlaying(maybeIndex)),
            A2($elm$html$Html$input, _List_fromArray([
                $elm$html$Html$Attributes$type_("range"),
                A2($elm$html$Html$Attributes$style, "width", "calc(100% - 56px)"),
                A2($elm$html$Html$Attributes$style, "height", "36px"),
                A2($elm$html$Html$Attributes$style, "margin", "0 10px"),
                $elm$html$Html$Attributes$min("0"),
                $elm$html$Html$Attributes$max($elm$core$String$fromInt(lastIndex)),
                $elm$html$Html$Attributes$value($elm$core$String$fromInt(selectedIndex)),
                $elm$html$Html$Events$onInput(A2($elm$core$Basics$composeR, $elm$core$String$toInt, A2($elm$core$Basics$composeR, $elm$core$Maybe$withDefault(lastIndex), $elm$browser$Debugger$Main$SliderJump)))
            ]), _List_Nil)
        ]));
    });
    var $elm$browser$Debugger$Main$viewHistory = F3(function(maybeIndex, history1, layout) {
        var block = $elm$browser$Debugger$Main$toMouseBlocker(layout);
        var _v0 = $elm$browser$Debugger$Main$toHistoryPercents(layout);
        var w = _v0.a;
        var h = _v0.b;
        return A2($elm$html$Html$div, _List_fromArray([
            A2($elm$html$Html$Attributes$style, "width", w),
            A2($elm$html$Html$Attributes$style, "height", h),
            A2($elm$html$Html$Attributes$style, "display", "flex"),
            A2($elm$html$Html$Attributes$style, "flex-direction", "column"),
            A2($elm$html$Html$Attributes$style, "color", "#DDDDDD"),
            A2($elm$html$Html$Attributes$style, "background-color", "rgb(61, 61, 61)"),
            A2($elm$html$Html$Attributes$style, "pointer-events", block),
            A2($elm$html$Html$Attributes$style, "user-select", block)
        ]), _List_fromArray([
            A2($elm$browser$Debugger$Main$viewHistorySlider, history1, maybeIndex),
            A2($elm$html$Html$map, $elm$browser$Debugger$Main$Jump, A2($elm$browser$Debugger$History$view, maybeIndex, history1)),
            A2($elm$html$Html$Lazy$lazy, $elm$browser$Debugger$Main$viewHistoryOptions, layout)
        ]));
    });
    var $elm$browser$Debugger$Main$popoutView = function(model) {
        var maybeIndex = function() {
            var _v0 = model.state;
            if (_v0.$ === "Running") return $elm$core$Maybe$Nothing;
            else {
                var index = _v0.a;
                return $elm$core$Maybe$Just(index);
            }
        }();
        var historyToRender = $elm$browser$Debugger$Main$cachedHistory(model);
        return A3($elm$html$Html$node, "body", _Utils_ap($elm$browser$Debugger$Main$toDragListeners(model.layout), _List_fromArray([
            A2($elm$html$Html$Attributes$style, "margin", "0"),
            A2($elm$html$Html$Attributes$style, "padding", "0"),
            A2($elm$html$Html$Attributes$style, "width", "100%"),
            A2($elm$html$Html$Attributes$style, "height", "100%"),
            A2($elm$html$Html$Attributes$style, "font-family", "monospace"),
            A2($elm$html$Html$Attributes$style, "display", "flex"),
            A2($elm$html$Html$Attributes$style, "flex-direction", $elm$browser$Debugger$Main$toFlexDirection(model.layout))
        ])), _List_fromArray([
            A3($elm$browser$Debugger$Main$viewHistory, maybeIndex, historyToRender, model.layout),
            $elm$browser$Debugger$Main$viewDragZone(model.layout),
            A3($elm$browser$Debugger$Main$viewExpando, model.expandoMsg, model.expandoModel, model.layout)
        ]));
    };
    var $elm$browser$Debugger$Overlay$BlockAll = {
        $: "BlockAll"
    };
    var $elm$browser$Debugger$Overlay$toBlockerType = F2(function(isPaused, state) {
        switch(state.$){
            case "None":
                return isPaused ? $elm$browser$Debugger$Overlay$BlockAll : $elm$browser$Debugger$Overlay$BlockNone;
            case "BadMetadata":
                return $elm$browser$Debugger$Overlay$BlockMost;
            case "BadImport":
                return $elm$browser$Debugger$Overlay$BlockMost;
            default:
                return $elm$browser$Debugger$Overlay$BlockMost;
        }
    });
    var $elm$browser$Debugger$Main$toBlockerType = function(model) {
        return A2($elm$browser$Debugger$Overlay$toBlockerType, $elm$browser$Debugger$Main$isPaused(model.state), model.overlay);
    };
    var $elm$browser$Debugger$Main$Horizontal = F3(function(a, b, c) {
        return {
            $: "Horizontal",
            a: a,
            b: b,
            c: c
        };
    });
    var $elm$browser$Debugger$Main$Running = function(a) {
        return {
            $: "Running",
            a: a
        };
    };
    var $elm$browser$Debugger$Main$Static = {
        $: "Static"
    };
    var $elm$browser$Debugger$Metadata$Error = F2(function(message, problems) {
        return {
            message: message,
            problems: problems
        };
    });
    var $elm$json$Json$Decode$decodeValue = _Json_run;
    var $elm$browser$Debugger$Metadata$Metadata = F2(function(versions, types) {
        return {
            types: types,
            versions: versions
        };
    });
    var $elm$browser$Debugger$Metadata$Types = F3(function(message, aliases, unions) {
        return {
            aliases: aliases,
            message: message,
            unions: unions
        };
    });
    var $elm$browser$Debugger$Metadata$Alias = F2(function(args, tipe) {
        return {
            args: args,
            tipe: tipe
        };
    });
    var $elm$json$Json$Decode$list = _Json_decodeList;
    var $elm$browser$Debugger$Metadata$decodeAlias = A3($elm$json$Json$Decode$map2, $elm$browser$Debugger$Metadata$Alias, A2($elm$json$Json$Decode$field, "args", $elm$json$Json$Decode$list($elm$json$Json$Decode$string)), A2($elm$json$Json$Decode$field, "type", $elm$json$Json$Decode$string));
    var $elm$browser$Debugger$Metadata$Union = F2(function(args, tags) {
        return {
            args: args,
            tags: tags
        };
    });
    var $elm$core$Dict$fromList = function(assocs) {
        return A3($elm$core$List$foldl, F2(function(_v0, dict) {
            var key = _v0.a;
            var value = _v0.b;
            return A3($elm$core$Dict$insert, key, value, dict);
        }), $elm$core$Dict$empty, assocs);
    };
    var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
    var $elm$json$Json$Decode$dict = function(decoder) {
        return A2($elm$json$Json$Decode$map, $elm$core$Dict$fromList, $elm$json$Json$Decode$keyValuePairs(decoder));
    };
    var $elm$browser$Debugger$Metadata$decodeUnion = A3($elm$json$Json$Decode$map2, $elm$browser$Debugger$Metadata$Union, A2($elm$json$Json$Decode$field, "args", $elm$json$Json$Decode$list($elm$json$Json$Decode$string)), A2($elm$json$Json$Decode$field, "tags", $elm$json$Json$Decode$dict($elm$json$Json$Decode$list($elm$json$Json$Decode$string))));
    var $elm$json$Json$Decode$map3 = _Json_map3;
    var $elm$browser$Debugger$Metadata$decodeTypes = A4($elm$json$Json$Decode$map3, $elm$browser$Debugger$Metadata$Types, A2($elm$json$Json$Decode$field, "message", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "aliases", $elm$json$Json$Decode$dict($elm$browser$Debugger$Metadata$decodeAlias)), A2($elm$json$Json$Decode$field, "unions", $elm$json$Json$Decode$dict($elm$browser$Debugger$Metadata$decodeUnion)));
    var $elm$browser$Debugger$Metadata$Versions = function(elm) {
        return {
            elm: elm
        };
    };
    var $elm$browser$Debugger$Metadata$decodeVersions = A2($elm$json$Json$Decode$map, $elm$browser$Debugger$Metadata$Versions, A2($elm$json$Json$Decode$field, "elm", $elm$json$Json$Decode$string));
    var $elm$browser$Debugger$Metadata$decoder = A3($elm$json$Json$Decode$map2, $elm$browser$Debugger$Metadata$Metadata, A2($elm$json$Json$Decode$field, "versions", $elm$browser$Debugger$Metadata$decodeVersions), A2($elm$json$Json$Decode$field, "types", $elm$browser$Debugger$Metadata$decodeTypes));
    var $elm$browser$Debugger$Metadata$ProblemType = F2(function(name, problems) {
        return {
            name: name,
            problems: problems
        };
    });
    var $elm$core$List$maybeCons = F3(function(f, mx, xs) {
        var _v0 = f(mx);
        if (_v0.$ === "Just") {
            var x = _v0.a;
            return A2($elm$core$List$cons, x, xs);
        } else return xs;
    });
    var $elm$core$List$filterMap = F2(function(f, xs) {
        return A3($elm$core$List$foldr, $elm$core$List$maybeCons(f), _List_Nil, xs);
    });
    var $elm$core$String$contains = _String_contains;
    var $elm$browser$Debugger$Metadata$hasProblem = F2(function(tipe, _v0) {
        var problem = _v0.a;
        var token = _v0.b;
        return A2($elm$core$String$contains, token, tipe) ? $elm$core$Maybe$Just(problem) : $elm$core$Maybe$Nothing;
    });
    var $elm$browser$Debugger$Metadata$Decoder = {
        $: "Decoder"
    };
    var $elm$browser$Debugger$Metadata$Function = {
        $: "Function"
    };
    var $elm$browser$Debugger$Metadata$Process = {
        $: "Process"
    };
    var $elm$browser$Debugger$Metadata$Program = {
        $: "Program"
    };
    var $elm$browser$Debugger$Metadata$Request = {
        $: "Request"
    };
    var $elm$browser$Debugger$Metadata$Socket = {
        $: "Socket"
    };
    var $elm$browser$Debugger$Metadata$Task = {
        $: "Task"
    };
    var $elm$browser$Debugger$Metadata$VirtualDom = {
        $: "VirtualDom"
    };
    var $elm$browser$Debugger$Metadata$problemTable = _List_fromArray([
        _Utils_Tuple2($elm$browser$Debugger$Metadata$Function, "->"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$Decoder, "Json.Decode.Decoder"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$Task, "Task.Task"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$Process, "Process.Id"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$Socket, "WebSocket.LowLevel.WebSocket"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$Request, "Http.Request"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$Program, "Platform.Program"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$VirtualDom, "VirtualDom.Node"),
        _Utils_Tuple2($elm$browser$Debugger$Metadata$VirtualDom, "VirtualDom.Attribute")
    ]);
    var $elm$browser$Debugger$Metadata$findProblems = function(tipe) {
        return A2($elm$core$List$filterMap, $elm$browser$Debugger$Metadata$hasProblem(tipe), $elm$browser$Debugger$Metadata$problemTable);
    };
    var $elm$browser$Debugger$Metadata$collectBadAliases = F3(function(name, _v0, list) {
        var tipe = _v0.tipe;
        var _v1 = $elm$browser$Debugger$Metadata$findProblems(tipe);
        if (!_v1.b) return list;
        else {
            var problems = _v1;
            return A2($elm$core$List$cons, A2($elm$browser$Debugger$Metadata$ProblemType, name, problems), list);
        }
    });
    var $elm$core$List$append = F2(function(xs, ys) {
        if (!ys.b) return xs;
        else return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
    });
    var $elm$core$List$concat = function(lists) {
        return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
    };
    var $elm$core$List$concatMap = F2(function(f, list) {
        return $elm$core$List$concat(A2($elm$core$List$map, f, list));
    });
    var $elm$core$Dict$values = function(dict) {
        return A3($elm$core$Dict$foldr, F3(function(key, value, valueList) {
            return A2($elm$core$List$cons, value, valueList);
        }), _List_Nil, dict);
    };
    var $elm$browser$Debugger$Metadata$collectBadUnions = F3(function(name, _v0, list) {
        var tags = _v0.tags;
        var _v1 = A2($elm$core$List$concatMap, $elm$browser$Debugger$Metadata$findProblems, $elm$core$List$concat($elm$core$Dict$values(tags)));
        if (!_v1.b) return list;
        else {
            var problems = _v1;
            return A2($elm$core$List$cons, A2($elm$browser$Debugger$Metadata$ProblemType, name, problems), list);
        }
    });
    var $elm$core$Dict$foldl = F3(function(func, acc, dict) {
        foldl: while(true){
            if (dict.$ === "RBEmpty_elm_builtin") return acc;
            else {
                var key = dict.b;
                var value = dict.c;
                var left = dict.d;
                var right = dict.e;
                var $temp$func = func, $temp$acc = A3(func, key, value, A3($elm$core$Dict$foldl, func, acc, left)), $temp$dict = right;
                func = $temp$func;
                acc = $temp$acc;
                dict = $temp$dict;
                continue foldl;
            }
        }
    });
    var $elm$browser$Debugger$Metadata$isPortable = function(_v0) {
        var types = _v0.types;
        var badAliases = A3($elm$core$Dict$foldl, $elm$browser$Debugger$Metadata$collectBadAliases, _List_Nil, types.aliases);
        var _v1 = A3($elm$core$Dict$foldl, $elm$browser$Debugger$Metadata$collectBadUnions, badAliases, types.unions);
        if (!_v1.b) return $elm$core$Maybe$Nothing;
        else {
            var problems = _v1;
            return $elm$core$Maybe$Just(A2($elm$browser$Debugger$Metadata$Error, types.message, problems));
        }
    };
    var $elm$browser$Debugger$Metadata$decode = function(value) {
        var _v0 = A2($elm$json$Json$Decode$decodeValue, $elm$browser$Debugger$Metadata$decoder, value);
        if (_v0.$ === "Err") return $elm$core$Result$Err(A2($elm$browser$Debugger$Metadata$Error, "The compiler is generating bad metadata. This is a compiler bug!", _List_Nil));
        else {
            var metadata = _v0.a;
            var _v1 = $elm$browser$Debugger$Metadata$isPortable(metadata);
            if (_v1.$ === "Nothing") return $elm$core$Result$Ok(metadata);
            else {
                var error = _v1.a;
                return $elm$core$Result$Err(error);
            }
        }
    };
    var $elm$browser$Debugger$History$History = F3(function(snapshots, recent, numMessages) {
        return {
            numMessages: numMessages,
            recent: recent,
            snapshots: snapshots
        };
    });
    var $elm$browser$Debugger$History$RecentHistory = F3(function(model, messages, numMessages) {
        return {
            messages: messages,
            model: model,
            numMessages: numMessages
        };
    });
    var $elm$browser$Debugger$History$empty = function(model) {
        return A3($elm$browser$Debugger$History$History, $elm$core$Array$empty, A3($elm$browser$Debugger$History$RecentHistory, model, _List_Nil, 0), 0);
    };
    var $elm$core$Dict$map = F2(function(func, dict) {
        if (dict.$ === "RBEmpty_elm_builtin") return $elm$core$Dict$RBEmpty_elm_builtin;
        else {
            var color = dict.a;
            var key = dict.b;
            var value = dict.c;
            var left = dict.d;
            var right = dict.e;
            return A5($elm$core$Dict$RBNode_elm_builtin, color, key, A2(func, key, value), A2($elm$core$Dict$map, func, left), A2($elm$core$Dict$map, func, right));
        }
    });
    var $elm$core$Dict$sizeHelp = F2(function(n, dict) {
        sizeHelp: while(true){
            if (dict.$ === "RBEmpty_elm_builtin") return n;
            else {
                var left = dict.d;
                var right = dict.e;
                var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right), $temp$dict = left;
                n = $temp$n;
                dict = $temp$dict;
                continue sizeHelp;
            }
        }
    });
    var $elm$core$Dict$size = function(dict) {
        return A2($elm$core$Dict$sizeHelp, 0, dict);
    };
    var $elm$browser$Debugger$Expando$initHelp = F2(function(isOuter, expando) {
        switch(expando.$){
            case "S":
                return expando;
            case "Primitive":
                return expando;
            case "Sequence":
                var seqType = expando.a;
                var isClosed = expando.b;
                var items = expando.c;
                return isOuter ? A3($elm$browser$Debugger$Expando$Sequence, seqType, false, A2($elm$core$List$map, $elm$browser$Debugger$Expando$initHelp(false), items)) : $elm$core$List$length(items) <= 8 ? A3($elm$browser$Debugger$Expando$Sequence, seqType, false, items) : expando;
            case "Dictionary":
                var isClosed = expando.a;
                var keyValuePairs = expando.b;
                return isOuter ? A2($elm$browser$Debugger$Expando$Dictionary, false, A2($elm$core$List$map, function(_v1) {
                    var k = _v1.a;
                    var v = _v1.b;
                    return _Utils_Tuple2(k, A2($elm$browser$Debugger$Expando$initHelp, false, v));
                }, keyValuePairs)) : $elm$core$List$length(keyValuePairs) <= 8 ? A2($elm$browser$Debugger$Expando$Dictionary, false, keyValuePairs) : expando;
            case "Record":
                var isClosed = expando.a;
                var entries = expando.b;
                return isOuter ? A2($elm$browser$Debugger$Expando$Record, false, A2($elm$core$Dict$map, F2(function(_v2, v) {
                    return A2($elm$browser$Debugger$Expando$initHelp, false, v);
                }), entries)) : $elm$core$Dict$size(entries) <= 4 ? A2($elm$browser$Debugger$Expando$Record, false, entries) : expando;
            default:
                var maybeName = expando.a;
                var isClosed = expando.b;
                var args = expando.c;
                return isOuter ? A3($elm$browser$Debugger$Expando$Constructor, maybeName, false, A2($elm$core$List$map, $elm$browser$Debugger$Expando$initHelp(false), args)) : $elm$core$List$length(args) <= 4 ? A3($elm$browser$Debugger$Expando$Constructor, maybeName, false, args) : expando;
        }
    });
    var $elm$browser$Debugger$Expando$init = function(value) {
        return A2($elm$browser$Debugger$Expando$initHelp, true, _Debugger_init(value));
    };
    var $elm$core$Platform$Cmd$map = _Platform_map;
    var $elm$browser$Debugger$Overlay$None = {
        $: "None"
    };
    var $elm$browser$Debugger$Overlay$none = $elm$browser$Debugger$Overlay$None;
    var $elm$browser$Debugger$Main$wrapInit = F4(function(metadata, popout, init, flags) {
        var _v0 = init(flags);
        var userModel = _v0.a;
        var userCommands = _v0.b;
        return _Utils_Tuple2({
            expandoModel: $elm$browser$Debugger$Expando$init(userModel),
            expandoMsg: $elm$browser$Debugger$Expando$init(_Utils_Tuple0),
            history: $elm$browser$Debugger$History$empty(userModel),
            layout: A3($elm$browser$Debugger$Main$Horizontal, $elm$browser$Debugger$Main$Static, 0.3, 0.5),
            metadata: $elm$browser$Debugger$Metadata$decode(metadata),
            overlay: $elm$browser$Debugger$Overlay$none,
            popout: popout,
            state: $elm$browser$Debugger$Main$Running(userModel)
        }, A2($elm$core$Platform$Cmd$map, $elm$browser$Debugger$Main$UserMsg, userCommands));
    });
    var $elm$browser$Debugger$Main$getLatestModel = function(state) {
        if (state.$ === "Running") {
            var model = state.a;
            return model;
        } else {
            var model = state.c;
            return model;
        }
    };
    var $elm$core$Platform$Sub$map = _Platform_map;
    var $elm$browser$Debugger$Main$wrapSubs = F2(function(subscriptions, model) {
        return A2($elm$core$Platform$Sub$map, $elm$browser$Debugger$Main$UserMsg, subscriptions($elm$browser$Debugger$Main$getLatestModel(model.state)));
    });
    var $elm$browser$Debugger$Main$Moving = {
        $: "Moving"
    };
    var $elm$browser$Debugger$Main$Paused = F5(function(a, b, c, d, e) {
        return {
            $: "Paused",
            a: a,
            b: b,
            c: c,
            d: d,
            e: e
        };
    });
    var $elm$browser$Debugger$History$Snapshot = F2(function(model, messages) {
        return {
            messages: messages,
            model: model
        };
    });
    var $elm$browser$Debugger$History$addRecent = F3(function(msg, newModel, _v0) {
        var model = _v0.model;
        var messages = _v0.messages;
        var numMessages = _v0.numMessages;
        return _Utils_eq(numMessages, $elm$browser$Debugger$History$maxSnapshotSize) ? _Utils_Tuple2($elm$core$Maybe$Just(A2($elm$browser$Debugger$History$Snapshot, model, $elm$core$Array$fromList(messages))), A3($elm$browser$Debugger$History$RecentHistory, newModel, _List_fromArray([
            msg
        ]), 1)) : _Utils_Tuple2($elm$core$Maybe$Nothing, A3($elm$browser$Debugger$History$RecentHistory, model, A2($elm$core$List$cons, msg, messages), numMessages + 1));
    });
    var $elm$core$Elm$JsArray$push = _JsArray_push;
    var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
    var $elm$core$Array$insertTailInTree = F4(function(shift, index, tail, tree) {
        var pos = $elm$core$Array$bitMask & index >>> shift;
        if (_Utils_cmp(pos, $elm$core$Elm$JsArray$length(tree)) > -1) {
            if (shift === 5) return A2($elm$core$Elm$JsArray$push, $elm$core$Array$Leaf(tail), tree);
            else {
                var newSub = $elm$core$Array$SubTree(A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
                return A2($elm$core$Elm$JsArray$push, newSub, tree);
            }
        } else {
            var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
            if (value.$ === "SubTree") {
                var subTree = value.a;
                var newSub = $elm$core$Array$SubTree(A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
                return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
            } else {
                var newSub = $elm$core$Array$SubTree(A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$singleton(value)));
                return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
            }
        }
    });
    var $elm$core$Array$unsafeReplaceTail = F2(function(newTail, _v0) {
        var len = _v0.a;
        var startShift = _v0.b;
        var tree = _v0.c;
        var tail = _v0.d;
        var originalTailLen = $elm$core$Elm$JsArray$length(tail);
        var newTailLen = $elm$core$Elm$JsArray$length(newTail);
        var newArrayLen = len + (newTailLen - originalTailLen);
        if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
            var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
            if (overflow) {
                var newShift = startShift + $elm$core$Array$shiftStep;
                var newTree = A4($elm$core$Array$insertTailInTree, newShift, len, newTail, $elm$core$Elm$JsArray$singleton($elm$core$Array$SubTree(tree)));
                return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
            } else return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree), $elm$core$Elm$JsArray$empty);
        } else return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
    });
    var $elm$core$Array$push = F2(function(a, array) {
        var tail = array.d;
        return A2($elm$core$Array$unsafeReplaceTail, A2($elm$core$Elm$JsArray$push, a, tail), array);
    });
    var $elm$browser$Debugger$History$add = F3(function(msg, model, _v0) {
        var snapshots = _v0.snapshots;
        var recent = _v0.recent;
        var numMessages = _v0.numMessages;
        var _v1 = A3($elm$browser$Debugger$History$addRecent, msg, model, recent);
        if (_v1.a.$ === "Just") {
            var snapshot = _v1.a.a;
            var newRecent = _v1.b;
            return A3($elm$browser$Debugger$History$History, A2($elm$core$Array$push, snapshot, snapshots), newRecent, numMessages + 1);
        } else {
            var _v2 = _v1.a;
            var newRecent = _v1.b;
            return A3($elm$browser$Debugger$History$History, snapshots, newRecent, numMessages + 1);
        }
    });
    var $elm$core$Basics$always = F2(function(a, _v0) {
        return a;
    });
    var $elm$browser$Debugger$Overlay$BadImport = function(a) {
        return {
            $: "BadImport",
            a: a
        };
    };
    var $elm$browser$Debugger$Overlay$RiskyImport = F2(function(a, b) {
        return {
            $: "RiskyImport",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$Report$VersionChanged = F2(function(a, b) {
        return {
            $: "VersionChanged",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$Report$MessageChanged = F2(function(a, b) {
        return {
            $: "MessageChanged",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$Report$SomethingChanged = function(a) {
        return {
            $: "SomethingChanged",
            a: a
        };
    };
    var $elm$browser$Debugger$Report$AliasChange = function(a) {
        return {
            $: "AliasChange",
            a: a
        };
    };
    var $elm$browser$Debugger$Metadata$checkAlias = F4(function(name, old, _new, changes) {
        return _Utils_eq(old.tipe, _new.tipe) && _Utils_eq(old.args, _new.args) ? changes : A2($elm$core$List$cons, $elm$browser$Debugger$Report$AliasChange(name), changes);
    });
    var $elm$browser$Debugger$Report$UnionChange = F2(function(a, b) {
        return {
            $: "UnionChange",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$Metadata$addTag = F3(function(tag, _v0, changes) {
        return _Utils_update(changes, {
            added: A2($elm$core$List$cons, tag, changes.added)
        });
    });
    var $elm$browser$Debugger$Metadata$checkTag = F4(function(tag, old, _new, changes) {
        return _Utils_eq(old, _new) ? changes : _Utils_update(changes, {
            changed: A2($elm$core$List$cons, tag, changes.changed)
        });
    });
    var $elm$browser$Debugger$Report$TagChanges = F4(function(removed, changed, added, argsMatch) {
        return {
            added: added,
            argsMatch: argsMatch,
            changed: changed,
            removed: removed
        };
    });
    var $elm$browser$Debugger$Report$emptyTagChanges = function(argsMatch) {
        return A4($elm$browser$Debugger$Report$TagChanges, _List_Nil, _List_Nil, _List_Nil, argsMatch);
    };
    var $elm$browser$Debugger$Report$hasTagChanges = function(tagChanges) {
        return _Utils_eq(tagChanges, A4($elm$browser$Debugger$Report$TagChanges, _List_Nil, _List_Nil, _List_Nil, true));
    };
    var $elm$core$Dict$merge = F6(function(leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
        var stepState = F3(function(rKey, rValue, _v0) {
            stepState: while(true){
                var list = _v0.a;
                var result = _v0.b;
                if (!list.b) return _Utils_Tuple2(list, A3(rightStep, rKey, rValue, result));
                else {
                    var _v2 = list.a;
                    var lKey = _v2.a;
                    var lValue = _v2.b;
                    var rest = list.b;
                    if (_Utils_cmp(lKey, rKey) < 0) {
                        var $temp$rKey = rKey, $temp$rValue = rValue, $temp$_v0 = _Utils_Tuple2(rest, A3(leftStep, lKey, lValue, result));
                        rKey = $temp$rKey;
                        rValue = $temp$rValue;
                        _v0 = $temp$_v0;
                        continue stepState;
                    } else {
                        if (_Utils_cmp(lKey, rKey) > 0) return _Utils_Tuple2(list, A3(rightStep, rKey, rValue, result));
                        else return _Utils_Tuple2(rest, A4(bothStep, lKey, lValue, rValue, result));
                    }
                }
            }
        });
        var _v3 = A3($elm$core$Dict$foldl, stepState, _Utils_Tuple2($elm$core$Dict$toList(leftDict), initialResult), rightDict);
        var leftovers = _v3.a;
        var intermediateResult = _v3.b;
        return A3($elm$core$List$foldl, F2(function(_v4, result) {
            var k = _v4.a;
            var v = _v4.b;
            return A3(leftStep, k, v, result);
        }), intermediateResult, leftovers);
    });
    var $elm$browser$Debugger$Metadata$removeTag = F3(function(tag, _v0, changes) {
        return _Utils_update(changes, {
            removed: A2($elm$core$List$cons, tag, changes.removed)
        });
    });
    var $elm$browser$Debugger$Metadata$checkUnion = F4(function(name, old, _new, changes) {
        var tagChanges = A6($elm$core$Dict$merge, $elm$browser$Debugger$Metadata$removeTag, $elm$browser$Debugger$Metadata$checkTag, $elm$browser$Debugger$Metadata$addTag, old.tags, _new.tags, $elm$browser$Debugger$Report$emptyTagChanges(_Utils_eq(old.args, _new.args)));
        return $elm$browser$Debugger$Report$hasTagChanges(tagChanges) ? changes : A2($elm$core$List$cons, A2($elm$browser$Debugger$Report$UnionChange, name, tagChanges), changes);
    });
    var $elm$browser$Debugger$Metadata$ignore = F3(function(key, value, report) {
        return report;
    });
    var $elm$browser$Debugger$Metadata$checkTypes = F2(function(old, _new) {
        return !_Utils_eq(old.message, _new.message) ? A2($elm$browser$Debugger$Report$MessageChanged, old.message, _new.message) : $elm$browser$Debugger$Report$SomethingChanged(A6($elm$core$Dict$merge, $elm$browser$Debugger$Metadata$ignore, $elm$browser$Debugger$Metadata$checkUnion, $elm$browser$Debugger$Metadata$ignore, old.unions, _new.unions, A6($elm$core$Dict$merge, $elm$browser$Debugger$Metadata$ignore, $elm$browser$Debugger$Metadata$checkAlias, $elm$browser$Debugger$Metadata$ignore, old.aliases, _new.aliases, _List_Nil)));
    });
    var $elm$browser$Debugger$Metadata$check = F2(function(old, _new) {
        return !_Utils_eq(old.versions.elm, _new.versions.elm) ? A2($elm$browser$Debugger$Report$VersionChanged, old.versions.elm, _new.versions.elm) : A2($elm$browser$Debugger$Metadata$checkTypes, old.types, _new.types);
    });
    var $elm$browser$Debugger$Report$CorruptHistory = {
        $: "CorruptHistory"
    };
    var $elm$browser$Debugger$Overlay$corruptImport = $elm$browser$Debugger$Overlay$BadImport($elm$browser$Debugger$Report$CorruptHistory);
    var $elm$json$Json$Decode$decodeString = _Json_runOnString;
    var $elm$browser$Debugger$Report$Fine = {
        $: "Fine"
    };
    var $elm$browser$Debugger$Report$Impossible = {
        $: "Impossible"
    };
    var $elm$browser$Debugger$Report$Risky = {
        $: "Risky"
    };
    var $elm$core$Basics$not = _Basics_not;
    var $elm$core$List$isEmpty = function(xs) {
        if (!xs.b) return true;
        else return false;
    };
    var $elm$browser$Debugger$Report$some = function(list) {
        return !$elm$core$List$isEmpty(list);
    };
    var $elm$browser$Debugger$Report$evaluateChange = function(change) {
        if (change.$ === "AliasChange") return $elm$browser$Debugger$Report$Impossible;
        else {
            var removed = change.b.removed;
            var changed = change.b.changed;
            var added = change.b.added;
            var argsMatch = change.b.argsMatch;
            return !argsMatch || $elm$browser$Debugger$Report$some(changed) || $elm$browser$Debugger$Report$some(removed) ? $elm$browser$Debugger$Report$Impossible : $elm$browser$Debugger$Report$some(added) ? $elm$browser$Debugger$Report$Risky : $elm$browser$Debugger$Report$Fine;
        }
    };
    var $elm$browser$Debugger$Report$worstCase = F2(function(status, statusList) {
        worstCase: while(true){
            if (!statusList.b) return status;
            else switch(statusList.a.$){
                case "Impossible":
                    var _v1 = statusList.a;
                    return $elm$browser$Debugger$Report$Impossible;
                case "Risky":
                    var _v2 = statusList.a;
                    var rest = statusList.b;
                    var $temp$status = $elm$browser$Debugger$Report$Risky, $temp$statusList = rest;
                    status = $temp$status;
                    statusList = $temp$statusList;
                    continue worstCase;
                default:
                    var _v3 = statusList.a;
                    var rest = statusList.b;
                    var $temp$status = status, $temp$statusList = rest;
                    status = $temp$status;
                    statusList = $temp$statusList;
                    continue worstCase;
            }
        }
    });
    var $elm$browser$Debugger$Report$evaluate = function(report) {
        switch(report.$){
            case "CorruptHistory":
                return $elm$browser$Debugger$Report$Impossible;
            case "VersionChanged":
                return $elm$browser$Debugger$Report$Impossible;
            case "MessageChanged":
                return $elm$browser$Debugger$Report$Impossible;
            default:
                var changes = report.a;
                return A2($elm$browser$Debugger$Report$worstCase, $elm$browser$Debugger$Report$Fine, A2($elm$core$List$map, $elm$browser$Debugger$Report$evaluateChange, changes));
        }
    };
    var $elm$json$Json$Decode$value = _Json_decodeValue;
    var $elm$browser$Debugger$Overlay$uploadDecoder = A3($elm$json$Json$Decode$map2, F2(function(x, y) {
        return _Utils_Tuple2(x, y);
    }), A2($elm$json$Json$Decode$field, "metadata", $elm$browser$Debugger$Metadata$decoder), A2($elm$json$Json$Decode$field, "history", $elm$json$Json$Decode$value));
    var $elm$browser$Debugger$Overlay$assessImport = F2(function(metadata, jsonString) {
        var _v0 = A2($elm$json$Json$Decode$decodeString, $elm$browser$Debugger$Overlay$uploadDecoder, jsonString);
        if (_v0.$ === "Err") return $elm$core$Result$Err($elm$browser$Debugger$Overlay$corruptImport);
        else {
            var _v1 = _v0.a;
            var foreignMetadata = _v1.a;
            var rawHistory = _v1.b;
            var report = A2($elm$browser$Debugger$Metadata$check, foreignMetadata, metadata);
            var _v2 = $elm$browser$Debugger$Report$evaluate(report);
            switch(_v2.$){
                case "Impossible":
                    return $elm$core$Result$Err($elm$browser$Debugger$Overlay$BadImport(report));
                case "Risky":
                    return $elm$core$Result$Err(A2($elm$browser$Debugger$Overlay$RiskyImport, report, rawHistory));
                default:
                    return $elm$core$Result$Ok(rawHistory);
            }
        }
    });
    var $elm$core$Platform$Cmd$batch = _Platform_batch;
    var $elm$browser$Debugger$Overlay$close = F2(function(msg, state) {
        switch(state.$){
            case "None":
                return $elm$core$Maybe$Nothing;
            case "BadMetadata":
                return $elm$core$Maybe$Nothing;
            case "BadImport":
                return $elm$core$Maybe$Nothing;
            default:
                var rawHistory = state.b;
                if (msg.$ === "Cancel") return $elm$core$Maybe$Nothing;
                else return $elm$core$Maybe$Just(rawHistory);
        }
    });
    var $elm$browser$Debugger$History$elmToJs = A2($elm$core$Basics$composeR, _Json_wrap, _Debugger_unsafeCoerce);
    var $elm$browser$Debugger$History$encodeHelp = F2(function(snapshot, allMessages) {
        return A3($elm$core$Array$foldl, $elm$core$List$cons, allMessages, snapshot.messages);
    });
    var $elm$json$Json$Encode$list = F2(function(func, entries) {
        return _Json_wrap(A3($elm$core$List$foldl, _Json_addEntry(func), _Json_emptyArray(_Utils_Tuple0), entries));
    });
    var $elm$browser$Debugger$History$encode = function(_v0) {
        var snapshots = _v0.snapshots;
        var recent = _v0.recent;
        return A2($elm$json$Json$Encode$list, $elm$browser$Debugger$History$elmToJs, A3($elm$core$Array$foldr, $elm$browser$Debugger$History$encodeHelp, $elm$core$List$reverse(recent.messages), snapshots));
    };
    var $elm$json$Json$Encode$object = function(pairs) {
        return _Json_wrap(A3($elm$core$List$foldl, F2(function(_v0, obj) {
            var k = _v0.a;
            var v = _v0.b;
            return A3(_Json_addField, k, v, obj);
        }), _Json_emptyObject(_Utils_Tuple0), pairs));
    };
    var $elm$browser$Debugger$Metadata$encodeAlias = function(_v0) {
        var args = _v0.args;
        var tipe = _v0.tipe;
        return $elm$json$Json$Encode$object(_List_fromArray([
            _Utils_Tuple2("args", A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, args)),
            _Utils_Tuple2("type", $elm$json$Json$Encode$string(tipe))
        ]));
    };
    var $elm$browser$Debugger$Metadata$encodeDict = F2(function(f, dict) {
        return $elm$json$Json$Encode$object($elm$core$Dict$toList(A2($elm$core$Dict$map, F2(function(key, value) {
            return f(value);
        }), dict)));
    });
    var $elm$browser$Debugger$Metadata$encodeUnion = function(_v0) {
        var args = _v0.args;
        var tags = _v0.tags;
        return $elm$json$Json$Encode$object(_List_fromArray([
            _Utils_Tuple2("args", A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, args)),
            _Utils_Tuple2("tags", A2($elm$browser$Debugger$Metadata$encodeDict, $elm$json$Json$Encode$list($elm$json$Json$Encode$string), tags))
        ]));
    };
    var $elm$browser$Debugger$Metadata$encodeTypes = function(_v0) {
        var message = _v0.message;
        var unions = _v0.unions;
        var aliases = _v0.aliases;
        return $elm$json$Json$Encode$object(_List_fromArray([
            _Utils_Tuple2("message", $elm$json$Json$Encode$string(message)),
            _Utils_Tuple2("aliases", A2($elm$browser$Debugger$Metadata$encodeDict, $elm$browser$Debugger$Metadata$encodeAlias, aliases)),
            _Utils_Tuple2("unions", A2($elm$browser$Debugger$Metadata$encodeDict, $elm$browser$Debugger$Metadata$encodeUnion, unions))
        ]));
    };
    var $elm$browser$Debugger$Metadata$encodeVersions = function(_v0) {
        var elm = _v0.elm;
        return $elm$json$Json$Encode$object(_List_fromArray([
            _Utils_Tuple2("elm", $elm$json$Json$Encode$string(elm))
        ]));
    };
    var $elm$browser$Debugger$Metadata$encode = function(_v0) {
        var versions = _v0.versions;
        var types = _v0.types;
        return $elm$json$Json$Encode$object(_List_fromArray([
            _Utils_Tuple2("versions", $elm$browser$Debugger$Metadata$encodeVersions(versions)),
            _Utils_Tuple2("types", $elm$browser$Debugger$Metadata$encodeTypes(types))
        ]));
    };
    var $elm$core$Basics$identity = function(x) {
        return x;
    };
    var $elm$core$Task$Perform = function(a) {
        return {
            $: "Perform",
            a: a
        };
    };
    var $elm$core$Task$succeed = _Scheduler_succeed;
    var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
    var $elm$core$Task$andThen = _Scheduler_andThen;
    var $elm$core$Task$map = F2(function(func, taskA) {
        return A2($elm$core$Task$andThen, function(a) {
            return $elm$core$Task$succeed(func(a));
        }, taskA);
    });
    var $elm$core$Task$map2 = F3(function(func, taskA, taskB) {
        return A2($elm$core$Task$andThen, function(a) {
            return A2($elm$core$Task$andThen, function(b) {
                return $elm$core$Task$succeed(A2(func, a, b));
            }, taskB);
        }, taskA);
    });
    var $elm$core$Task$sequence = function(tasks) {
        return A3($elm$core$List$foldr, $elm$core$Task$map2($elm$core$List$cons), $elm$core$Task$succeed(_List_Nil), tasks);
    };
    var $elm$core$Platform$sendToApp = _Platform_sendToApp;
    var $elm$core$Task$spawnCmd = F2(function(router, _v0) {
        var task = _v0.a;
        return _Scheduler_spawn(A2($elm$core$Task$andThen, $elm$core$Platform$sendToApp(router), task));
    });
    var $elm$core$Task$onEffects = F3(function(router, commands, state) {
        return A2($elm$core$Task$map, function(_v0) {
            return _Utils_Tuple0;
        }, $elm$core$Task$sequence(A2($elm$core$List$map, $elm$core$Task$spawnCmd(router), commands)));
    });
    var $elm$core$Task$onSelfMsg = F3(function(_v0, _v1, _v2) {
        return $elm$core$Task$succeed(_Utils_Tuple0);
    });
    var $elm$core$Task$cmdMap = F2(function(tagger, _v0) {
        var task = _v0.a;
        return $elm$core$Task$Perform(A2($elm$core$Task$map, tagger, task));
    });
    _Platform_effectManagers["Task"] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
    var $elm$core$Task$command = _Platform_leaf("Task");
    var $elm$core$Task$perform = F2(function(toMessage, task) {
        return $elm$core$Task$command($elm$core$Task$Perform(A2($elm$core$Task$map, toMessage, task)));
    });
    var $elm$browser$Debugger$Main$download = F2(function(metadata, history1) {
        var historyLength = $elm$browser$Debugger$History$size(history1);
        return A2($elm$core$Task$perform, function(_v0) {
            return $elm$browser$Debugger$Main$NoOp;
        }, A2(_Debugger_download, historyLength, _Json_unwrap($elm$json$Json$Encode$object(_List_fromArray([
            _Utils_Tuple2("metadata", $elm$browser$Debugger$Metadata$encode(metadata)),
            _Utils_Tuple2("history", $elm$browser$Debugger$History$encode(history1))
        ])))));
    });
    var $elm$browser$Debugger$Main$Vertical = F3(function(a, b, c) {
        return {
            $: "Vertical",
            a: a,
            b: b,
            c: c
        };
    });
    var $elm$browser$Debugger$Main$drag = F2(function(info, layout) {
        if (layout.$ === "Horizontal") {
            var status = layout.a;
            var y = layout.c;
            return A3($elm$browser$Debugger$Main$Horizontal, status, info.x / info.width, y);
        } else {
            var status = layout.a;
            var x = layout.b;
            return A3($elm$browser$Debugger$Main$Vertical, status, x, info.y / info.height);
        }
    });
    var $elm$browser$Debugger$History$Stepping = F2(function(a, b) {
        return {
            $: "Stepping",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$History$Done = F2(function(a, b) {
        return {
            $: "Done",
            a: a,
            b: b
        };
    });
    var $elm$browser$Debugger$History$getHelp = F3(function(update, msg, getResult) {
        if (getResult.$ === "Done") return getResult;
        else {
            var n = getResult.a;
            var model = getResult.b;
            return !n ? A2($elm$browser$Debugger$History$Done, msg, A2(update, msg, model).a) : A2($elm$browser$Debugger$History$Stepping, n - 1, A2(update, msg, model).a);
        }
    });
    var $elm$browser$Debugger$History$undone = function(getResult) {
        undone: while(true)if (getResult.$ === "Done") {
            var msg = getResult.a;
            var model = getResult.b;
            return _Utils_Tuple2(model, msg);
        } else {
            var $temp$getResult = getResult;
            getResult = $temp$getResult;
            continue undone;
        }
    };
    var $elm$browser$Debugger$History$get = F3(function(update, index, history1) {
        get: while(true){
            var recent = history1.recent;
            var snapshotMax = history1.numMessages - recent.numMessages;
            if (_Utils_cmp(index, snapshotMax) > -1) return $elm$browser$Debugger$History$undone(A3($elm$core$List$foldr, $elm$browser$Debugger$History$getHelp(update), A2($elm$browser$Debugger$History$Stepping, index - snapshotMax, recent.model), recent.messages));
            else {
                var _v0 = A2($elm$core$Array$get, index / $elm$browser$Debugger$History$maxSnapshotSize | 0, history1.snapshots);
                if (_v0.$ === "Nothing") {
                    var $temp$update = update, $temp$index = index, $temp$history = history1;
                    update = $temp$update;
                    index = $temp$index;
                    history1 = $temp$history;
                    continue get;
                } else {
                    var model = _v0.a.model;
                    var messages = _v0.a.messages;
                    return $elm$browser$Debugger$History$undone(A3($elm$core$Array$foldr, $elm$browser$Debugger$History$getHelp(update), A2($elm$browser$Debugger$History$Stepping, index % $elm$browser$Debugger$History$maxSnapshotSize, model), messages));
                }
            }
        }
    });
    var $elm$browser$Debugger$History$getRecentMsg = function(history1) {
        getRecentMsg: while(true){
            var _v0 = history1.recent.messages;
            if (!_v0.b) {
                var $temp$history = history1;
                history1 = $temp$history;
                continue getRecentMsg;
            } else {
                var first = _v0.a;
                return first;
            }
        }
    };
    var $elm$core$Dict$get = F2(function(targetKey, dict) {
        get: while(true){
            if (dict.$ === "RBEmpty_elm_builtin") return $elm$core$Maybe$Nothing;
            else {
                var key = dict.b;
                var value = dict.c;
                var left = dict.d;
                var right = dict.e;
                var _v1 = A2($elm$core$Basics$compare, targetKey, key);
                switch(_v1.$){
                    case "LT":
                        var $temp$targetKey = targetKey, $temp$dict = left;
                        targetKey = $temp$targetKey;
                        dict = $temp$dict;
                        continue get;
                    case "EQ":
                        return $elm$core$Maybe$Just(value);
                    default:
                        var $temp$targetKey = targetKey, $temp$dict = right;
                        targetKey = $temp$targetKey;
                        dict = $temp$dict;
                        continue get;
                }
            }
        }
    });
    var $elm$browser$Debugger$Expando$mergeDictHelp = F3(function(oldDict, key, value) {
        var _v12 = A2($elm$core$Dict$get, key, oldDict);
        if (_v12.$ === "Nothing") return value;
        else {
            var oldValue = _v12.a;
            return A2($elm$browser$Debugger$Expando$mergeHelp, oldValue, value);
        }
    });
    var $elm$browser$Debugger$Expando$mergeHelp = F2(function(old, _new) {
        var _v3 = _Utils_Tuple2(old, _new);
        _v3$6: while(true)switch(_v3.b.$){
            case "S":
                return _new;
            case "Primitive":
                return _new;
            case "Sequence":
                if (_v3.a.$ === "Sequence") {
                    var _v4 = _v3.a;
                    var isClosed = _v4.b;
                    var oldValues = _v4.c;
                    var _v5 = _v3.b;
                    var seqType = _v5.a;
                    var newValues = _v5.c;
                    return A3($elm$browser$Debugger$Expando$Sequence, seqType, isClosed, A2($elm$browser$Debugger$Expando$mergeListHelp, oldValues, newValues));
                } else break _v3$6;
            case "Dictionary":
                if (_v3.a.$ === "Dictionary") {
                    var _v6 = _v3.a;
                    var isClosed = _v6.a;
                    var _v7 = _v3.b;
                    var keyValuePairs = _v7.b;
                    return A2($elm$browser$Debugger$Expando$Dictionary, isClosed, keyValuePairs);
                } else break _v3$6;
            case "Record":
                if (_v3.a.$ === "Record") {
                    var _v8 = _v3.a;
                    var isClosed = _v8.a;
                    var oldDict = _v8.b;
                    var _v9 = _v3.b;
                    var newDict = _v9.b;
                    return A2($elm$browser$Debugger$Expando$Record, isClosed, A2($elm$core$Dict$map, $elm$browser$Debugger$Expando$mergeDictHelp(oldDict), newDict));
                } else break _v3$6;
            default:
                if (_v3.a.$ === "Constructor") {
                    var _v10 = _v3.a;
                    var isClosed = _v10.b;
                    var oldValues = _v10.c;
                    var _v11 = _v3.b;
                    var maybeName = _v11.a;
                    var newValues = _v11.c;
                    return A3($elm$browser$Debugger$Expando$Constructor, maybeName, isClosed, A2($elm$browser$Debugger$Expando$mergeListHelp, oldValues, newValues));
                } else break _v3$6;
        }
        return _new;
    });
    var $elm$browser$Debugger$Expando$mergeListHelp = F2(function(olds, news) {
        var _v0 = _Utils_Tuple2(olds, news);
        if (!_v0.a.b) return news;
        else {
            if (!_v0.b.b) return news;
            else {
                var _v1 = _v0.a;
                var x = _v1.a;
                var xs = _v1.b;
                var _v2 = _v0.b;
                var y = _v2.a;
                var ys = _v2.b;
                return A2($elm$core$List$cons, A2($elm$browser$Debugger$Expando$mergeHelp, x, y), A2($elm$browser$Debugger$Expando$mergeListHelp, xs, ys));
            }
        }
    });
    var $elm$browser$Debugger$Expando$merge = F2(function(value, expando) {
        return A2($elm$browser$Debugger$Expando$mergeHelp, expando, _Debugger_init(value));
    });
    var $elm$browser$Debugger$Main$jumpUpdate = F3(function(update, index, model) {
        var history1 = $elm$browser$Debugger$Main$cachedHistory(model);
        var currentMsg = $elm$browser$Debugger$History$getRecentMsg(history1);
        var currentModel = $elm$browser$Debugger$Main$getLatestModel(model.state);
        var _v0 = A3($elm$browser$Debugger$History$get, update, index, history1);
        var indexModel = _v0.a;
        var indexMsg = _v0.b;
        return _Utils_update(model, {
            expandoModel: A2($elm$browser$Debugger$Expando$merge, indexModel, model.expandoModel),
            expandoMsg: A2($elm$browser$Debugger$Expando$merge, indexMsg, model.expandoMsg),
            state: A5($elm$browser$Debugger$Main$Paused, index, indexModel, currentModel, currentMsg, history1)
        });
    });
    var $elm$browser$Debugger$History$jsToElm = A2($elm$core$Basics$composeR, _Json_unwrap, _Debugger_unsafeCoerce);
    var $elm$browser$Debugger$History$decoder = F2(function(initialModel, update) {
        var addMessage = F2(function(rawMsg, _v0) {
            var model = _v0.a;
            var history1 = _v0.b;
            var msg = $elm$browser$Debugger$History$jsToElm(rawMsg);
            return _Utils_Tuple2(A2(update, msg, model), A3($elm$browser$Debugger$History$add, msg, model, history1));
        });
        var updateModel = function(rawMsgs) {
            return A3($elm$core$List$foldl, addMessage, _Utils_Tuple2(initialModel, $elm$browser$Debugger$History$empty(initialModel)), rawMsgs);
        };
        return A2($elm$json$Json$Decode$map, updateModel, $elm$json$Json$Decode$list($elm$json$Json$Decode$value));
    });
    var $elm$browser$Debugger$History$getInitialModel = function(_v0) {
        var snapshots = _v0.snapshots;
        var recent = _v0.recent;
        var _v1 = A2($elm$core$Array$get, 0, snapshots);
        if (_v1.$ === "Just") {
            var model = _v1.a.model;
            return model;
        } else return recent.model;
    };
    var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
    var $elm$browser$Debugger$Main$loadNewHistory = F3(function(rawHistory, update, model) {
        var pureUserUpdate = F2(function(msg, userModel) {
            return A2(update, msg, userModel).a;
        });
        var initialUserModel = $elm$browser$Debugger$History$getInitialModel(model.history);
        var decoder = A2($elm$browser$Debugger$History$decoder, initialUserModel, pureUserUpdate);
        var _v0 = A2($elm$json$Json$Decode$decodeValue, decoder, rawHistory);
        if (_v0.$ === "Err") return _Utils_Tuple2(_Utils_update(model, {
            overlay: $elm$browser$Debugger$Overlay$corruptImport
        }), $elm$core$Platform$Cmd$none);
        else {
            var _v1 = _v0.a;
            var latestUserModel = _v1.a;
            var newHistory = _v1.b;
            return _Utils_Tuple2(_Utils_update(model, {
                expandoModel: $elm$browser$Debugger$Expando$init(latestUserModel),
                expandoMsg: $elm$browser$Debugger$Expando$init($elm$browser$Debugger$History$getRecentMsg(newHistory)),
                history: newHistory,
                overlay: $elm$browser$Debugger$Overlay$none,
                state: $elm$browser$Debugger$Main$Running(latestUserModel)
            }), $elm$core$Platform$Cmd$none);
        }
    });
    var $elm$browser$Debugger$Main$scroll = function(popout) {
        return A2($elm$core$Task$perform, $elm$core$Basics$always($elm$browser$Debugger$Main$NoOp), _Debugger_scroll(popout));
    };
    var $elm$browser$Debugger$Main$scrollTo = F2(function(id, popout) {
        return A2($elm$core$Task$perform, $elm$core$Basics$always($elm$browser$Debugger$Main$NoOp), A2(_Debugger_scrollTo, id, popout));
    });
    var $elm$browser$Debugger$Main$setDragStatus = F2(function(status, layout) {
        if (layout.$ === "Horizontal") {
            var x = layout.b;
            var y = layout.c;
            return A3($elm$browser$Debugger$Main$Horizontal, status, x, y);
        } else {
            var x = layout.b;
            var y = layout.c;
            return A3($elm$browser$Debugger$Main$Vertical, status, x, y);
        }
    });
    var $elm$browser$Debugger$Main$swapLayout = function(layout) {
        if (layout.$ === "Horizontal") {
            var s = layout.a;
            var x = layout.b;
            var y = layout.c;
            return A3($elm$browser$Debugger$Main$Vertical, s, x, y);
        } else {
            var s = layout.a;
            var x = layout.b;
            var y = layout.c;
            return A3($elm$browser$Debugger$Main$Horizontal, s, x, y);
        }
    };
    var $elm$core$Dict$getMin = function(dict) {
        getMin: while(true){
            if (dict.$ === "RBNode_elm_builtin" && dict.d.$ === "RBNode_elm_builtin") {
                var left = dict.d;
                var $temp$dict = left;
                dict = $temp$dict;
                continue getMin;
            } else return dict;
        }
    };
    var $elm$core$Dict$moveRedLeft = function(dict) {
        if (dict.$ === "RBNode_elm_builtin" && dict.d.$ === "RBNode_elm_builtin" && dict.e.$ === "RBNode_elm_builtin") {
            if (dict.e.d.$ === "RBNode_elm_builtin" && dict.e.d.a.$ === "Red") {
                var clr = dict.a;
                var k = dict.b;
                var v = dict.c;
                var _v1 = dict.d;
                var lClr = _v1.a;
                var lK = _v1.b;
                var lV = _v1.c;
                var lLeft = _v1.d;
                var lRight = _v1.e;
                var _v2 = dict.e;
                var rClr = _v2.a;
                var rK = _v2.b;
                var rV = _v2.c;
                var rLeft = _v2.d;
                var _v3 = rLeft.a;
                var rlK = rLeft.b;
                var rlV = rLeft.c;
                var rlL = rLeft.d;
                var rlR = rLeft.e;
                var rRight = _v2.e;
                return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rlK, rlV, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight), rlL), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
            } else {
                var clr = dict.a;
                var k = dict.b;
                var v = dict.c;
                var _v4 = dict.d;
                var lClr = _v4.a;
                var lK = _v4.b;
                var lV = _v4.c;
                var lLeft = _v4.d;
                var lRight = _v4.e;
                var _v5 = dict.e;
                var rClr = _v5.a;
                var rK = _v5.b;
                var rV = _v5.c;
                var rLeft = _v5.d;
                var rRight = _v5.e;
                if (clr.$ === "Black") return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
                else return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
            }
        } else return dict;
    };
    var $elm$core$Dict$moveRedRight = function(dict) {
        if (dict.$ === "RBNode_elm_builtin" && dict.d.$ === "RBNode_elm_builtin" && dict.e.$ === "RBNode_elm_builtin") {
            if (dict.d.d.$ === "RBNode_elm_builtin" && dict.d.d.a.$ === "Red") {
                var clr = dict.a;
                var k = dict.b;
                var v = dict.c;
                var _v1 = dict.d;
                var lClr = _v1.a;
                var lK = _v1.b;
                var lV = _v1.c;
                var _v2 = _v1.d;
                var _v3 = _v2.a;
                var llK = _v2.b;
                var llV = _v2.c;
                var llLeft = _v2.d;
                var llRight = _v2.e;
                var lRight = _v1.e;
                var _v4 = dict.e;
                var rClr = _v4.a;
                var rK = _v4.b;
                var rV = _v4.c;
                var rLeft = _v4.d;
                var rRight = _v4.e;
                return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, lRight, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
            } else {
                var clr = dict.a;
                var k = dict.b;
                var v = dict.c;
                var _v5 = dict.d;
                var lClr = _v5.a;
                var lK = _v5.b;
                var lV = _v5.c;
                var lLeft = _v5.d;
                var lRight = _v5.e;
                var _v6 = dict.e;
                var rClr = _v6.a;
                var rK = _v6.b;
                var rV = _v6.c;
                var rLeft = _v6.d;
                var rRight = _v6.e;
                if (clr.$ === "Black") return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
                else return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
            }
        } else return dict;
    };
    var $elm$core$Dict$removeHelpPrepEQGT = F7(function(targetKey, dict, color, key, value, left, right) {
        if (left.$ === "RBNode_elm_builtin" && left.a.$ === "Red") {
            var _v1 = left.a;
            var lK = left.b;
            var lV = left.c;
            var lLeft = left.d;
            var lRight = left.e;
            return A5($elm$core$Dict$RBNode_elm_builtin, color, lK, lV, lLeft, A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
        } else {
            _v2$2: while(true){
                if (right.$ === "RBNode_elm_builtin" && right.a.$ === "Black") {
                    if (right.d.$ === "RBNode_elm_builtin") {
                        if (right.d.a.$ === "Black") {
                            var _v3 = right.a;
                            var _v4 = right.d;
                            var _v5 = _v4.a;
                            return $elm$core$Dict$moveRedRight(dict);
                        } else break _v2$2;
                    } else {
                        var _v6 = right.a;
                        var _v7 = right.d;
                        return $elm$core$Dict$moveRedRight(dict);
                    }
                } else break _v2$2;
            }
            return dict;
        }
    });
    var $elm$core$Dict$removeMin = function(dict) {
        if (dict.$ === "RBNode_elm_builtin" && dict.d.$ === "RBNode_elm_builtin") {
            var color = dict.a;
            var key = dict.b;
            var value = dict.c;
            var left = dict.d;
            var lColor = left.a;
            var lLeft = left.d;
            var right = dict.e;
            if (lColor.$ === "Black") {
                if (lLeft.$ === "RBNode_elm_builtin" && lLeft.a.$ === "Red") {
                    var _v3 = lLeft.a;
                    return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, $elm$core$Dict$removeMin(left), right);
                } else {
                    var _v4 = $elm$core$Dict$moveRedLeft(dict);
                    if (_v4.$ === "RBNode_elm_builtin") {
                        var nColor = _v4.a;
                        var nKey = _v4.b;
                        var nValue = _v4.c;
                        var nLeft = _v4.d;
                        var nRight = _v4.e;
                        return A5($elm$core$Dict$balance, nColor, nKey, nValue, $elm$core$Dict$removeMin(nLeft), nRight);
                    } else return $elm$core$Dict$RBEmpty_elm_builtin;
                }
            } else return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, $elm$core$Dict$removeMin(left), right);
        } else return $elm$core$Dict$RBEmpty_elm_builtin;
    };
    var $elm$core$Dict$removeHelp = F2(function(targetKey, dict) {
        if (dict.$ === "RBEmpty_elm_builtin") return $elm$core$Dict$RBEmpty_elm_builtin;
        else {
            var color = dict.a;
            var key = dict.b;
            var value = dict.c;
            var left = dict.d;
            var right = dict.e;
            if (_Utils_cmp(targetKey, key) < 0) {
                if (left.$ === "RBNode_elm_builtin" && left.a.$ === "Black") {
                    var _v4 = left.a;
                    var lLeft = left.d;
                    if (lLeft.$ === "RBNode_elm_builtin" && lLeft.a.$ === "Red") {
                        var _v6 = lLeft.a;
                        return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, A2($elm$core$Dict$removeHelp, targetKey, left), right);
                    } else {
                        var _v7 = $elm$core$Dict$moveRedLeft(dict);
                        if (_v7.$ === "RBNode_elm_builtin") {
                            var nColor = _v7.a;
                            var nKey = _v7.b;
                            var nValue = _v7.c;
                            var nLeft = _v7.d;
                            var nRight = _v7.e;
                            return A5($elm$core$Dict$balance, nColor, nKey, nValue, A2($elm$core$Dict$removeHelp, targetKey, nLeft), nRight);
                        } else return $elm$core$Dict$RBEmpty_elm_builtin;
                    }
                } else return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, A2($elm$core$Dict$removeHelp, targetKey, left), right);
            } else return A2($elm$core$Dict$removeHelpEQGT, targetKey, A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
        }
    });
    var $elm$core$Dict$removeHelpEQGT = F2(function(targetKey, dict) {
        if (dict.$ === "RBNode_elm_builtin") {
            var color = dict.a;
            var key = dict.b;
            var value = dict.c;
            var left = dict.d;
            var right = dict.e;
            if (_Utils_eq(targetKey, key)) {
                var _v1 = $elm$core$Dict$getMin(right);
                if (_v1.$ === "RBNode_elm_builtin") {
                    var minKey = _v1.b;
                    var minValue = _v1.c;
                    return A5($elm$core$Dict$balance, color, minKey, minValue, left, $elm$core$Dict$removeMin(right));
                } else return $elm$core$Dict$RBEmpty_elm_builtin;
            } else return A5($elm$core$Dict$balance, color, key, value, left, A2($elm$core$Dict$removeHelp, targetKey, right));
        } else return $elm$core$Dict$RBEmpty_elm_builtin;
    });
    var $elm$core$Dict$remove = F2(function(key, dict) {
        var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
        if (_v0.$ === "RBNode_elm_builtin" && _v0.a.$ === "Red") {
            var _v1 = _v0.a;
            var k = _v0.b;
            var v = _v0.c;
            var l = _v0.d;
            var r = _v0.e;
            return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
        } else {
            var x = _v0;
            return x;
        }
    });
    var $elm$core$Dict$update = F3(function(targetKey, alter, dictionary) {
        var _v0 = alter(A2($elm$core$Dict$get, targetKey, dictionary));
        if (_v0.$ === "Just") {
            var value = _v0.a;
            return A3($elm$core$Dict$insert, targetKey, value, dictionary);
        } else return A2($elm$core$Dict$remove, targetKey, dictionary);
    });
    var $elm$browser$Debugger$Expando$updateIndex = F3(function(n, func, list) {
        if (!list.b) return _List_Nil;
        else {
            var x = list.a;
            var xs = list.b;
            return n <= 0 ? A2($elm$core$List$cons, func(x), xs) : A2($elm$core$List$cons, x, A3($elm$browser$Debugger$Expando$updateIndex, n - 1, func, xs));
        }
    });
    var $elm$browser$Debugger$Expando$update = F2(function(msg, value) {
        switch(value.$){
            case "S":
                return value;
            case "Primitive":
                return value;
            case "Sequence":
                var seqType = value.a;
                var isClosed = value.b;
                var valueList = value.c;
                switch(msg.$){
                    case "Toggle":
                        return A3($elm$browser$Debugger$Expando$Sequence, seqType, !isClosed, valueList);
                    case "Index":
                        if (msg.a.$ === "None") {
                            var _v3 = msg.a;
                            var index = msg.b;
                            var subMsg = msg.c;
                            return A3($elm$browser$Debugger$Expando$Sequence, seqType, isClosed, A3($elm$browser$Debugger$Expando$updateIndex, index, $elm$browser$Debugger$Expando$update(subMsg), valueList));
                        } else return value;
                    default:
                        return value;
                }
            case "Dictionary":
                var isClosed = value.a;
                var keyValuePairs = value.b;
                switch(msg.$){
                    case "Toggle":
                        return A2($elm$browser$Debugger$Expando$Dictionary, !isClosed, keyValuePairs);
                    case "Index":
                        var redirect = msg.a;
                        var index = msg.b;
                        var subMsg = msg.c;
                        switch(redirect.$){
                            case "None":
                                return value;
                            case "Key":
                                return A2($elm$browser$Debugger$Expando$Dictionary, isClosed, A3($elm$browser$Debugger$Expando$updateIndex, index, function(_v6) {
                                    var k = _v6.a;
                                    var v = _v6.b;
                                    return _Utils_Tuple2(A2($elm$browser$Debugger$Expando$update, subMsg, k), v);
                                }, keyValuePairs));
                            default:
                                return A2($elm$browser$Debugger$Expando$Dictionary, isClosed, A3($elm$browser$Debugger$Expando$updateIndex, index, function(_v7) {
                                    var k = _v7.a;
                                    var v = _v7.b;
                                    return _Utils_Tuple2(k, A2($elm$browser$Debugger$Expando$update, subMsg, v));
                                }, keyValuePairs));
                        }
                    default:
                        return value;
                }
            case "Record":
                var isClosed = value.a;
                var valueDict = value.b;
                switch(msg.$){
                    case "Toggle":
                        return A2($elm$browser$Debugger$Expando$Record, !isClosed, valueDict);
                    case "Index":
                        return value;
                    default:
                        var field = msg.a;
                        var subMsg = msg.b;
                        return A2($elm$browser$Debugger$Expando$Record, isClosed, A3($elm$core$Dict$update, field, $elm$browser$Debugger$Expando$updateField(subMsg), valueDict));
                }
            default:
                var maybeName = value.a;
                var isClosed = value.b;
                var valueList = value.c;
                switch(msg.$){
                    case "Toggle":
                        return A3($elm$browser$Debugger$Expando$Constructor, maybeName, !isClosed, valueList);
                    case "Index":
                        if (msg.a.$ === "None") {
                            var _v10 = msg.a;
                            var index = msg.b;
                            var subMsg = msg.c;
                            return A3($elm$browser$Debugger$Expando$Constructor, maybeName, isClosed, A3($elm$browser$Debugger$Expando$updateIndex, index, $elm$browser$Debugger$Expando$update(subMsg), valueList));
                        } else return value;
                    default:
                        return value;
                }
        }
    });
    var $elm$browser$Debugger$Expando$updateField = F2(function(msg, maybeExpando) {
        if (maybeExpando.$ === "Nothing") return maybeExpando;
        else {
            var expando = maybeExpando.a;
            return $elm$core$Maybe$Just(A2($elm$browser$Debugger$Expando$update, msg, expando));
        }
    });
    var $elm$browser$Debugger$Main$Upload = function(a) {
        return {
            $: "Upload",
            a: a
        };
    };
    var $elm$browser$Debugger$Main$upload = function(popout) {
        return A2($elm$core$Task$perform, $elm$browser$Debugger$Main$Upload, _Debugger_upload(popout));
    };
    var $elm$browser$Debugger$Overlay$BadMetadata = function(a) {
        return {
            $: "BadMetadata",
            a: a
        };
    };
    var $elm$browser$Debugger$Overlay$badMetadata = $elm$browser$Debugger$Overlay$BadMetadata;
    var $elm$browser$Debugger$Main$withGoodMetadata = F2(function(model, func) {
        var _v0 = model.metadata;
        if (_v0.$ === "Ok") {
            var metadata = _v0.a;
            return func(metadata);
        } else {
            var error = _v0.a;
            return _Utils_Tuple2(_Utils_update(model, {
                overlay: $elm$browser$Debugger$Overlay$badMetadata(error)
            }), $elm$core$Platform$Cmd$none);
        }
    });
    var $elm$browser$Debugger$Main$wrapUpdate = F3(function(update, msg, model) {
        wrapUpdate: while(true)switch(msg.$){
            case "NoOp":
                return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
            case "UserMsg":
                var userMsg = msg.a;
                var userModel = $elm$browser$Debugger$Main$getLatestModel(model.state);
                var newHistory = A3($elm$browser$Debugger$History$add, userMsg, userModel, model.history);
                var _v1 = A2(update, userMsg, userModel);
                var newUserModel = _v1.a;
                var userCmds = _v1.b;
                var commands = A2($elm$core$Platform$Cmd$map, $elm$browser$Debugger$Main$UserMsg, userCmds);
                var _v2 = model.state;
                if (_v2.$ === "Running") return _Utils_Tuple2(_Utils_update(model, {
                    expandoModel: A2($elm$browser$Debugger$Expando$merge, newUserModel, model.expandoModel),
                    expandoMsg: A2($elm$browser$Debugger$Expando$merge, userMsg, model.expandoMsg),
                    history: newHistory,
                    state: $elm$browser$Debugger$Main$Running(newUserModel)
                }), $elm$core$Platform$Cmd$batch(_List_fromArray([
                    commands,
                    $elm$browser$Debugger$Main$scroll(model.popout)
                ])));
                else {
                    var index = _v2.a;
                    var indexModel = _v2.b;
                    var history1 = _v2.e;
                    return _Utils_Tuple2(_Utils_update(model, {
                        history: newHistory,
                        state: A5($elm$browser$Debugger$Main$Paused, index, indexModel, newUserModel, userMsg, history1)
                    }), commands);
                }
            case "TweakExpandoMsg":
                var eMsg = msg.a;
                return _Utils_Tuple2(_Utils_update(model, {
                    expandoMsg: A2($elm$browser$Debugger$Expando$update, eMsg, model.expandoMsg)
                }), $elm$core$Platform$Cmd$none);
            case "TweakExpandoModel":
                var eMsg = msg.a;
                return _Utils_Tuple2(_Utils_update(model, {
                    expandoModel: A2($elm$browser$Debugger$Expando$update, eMsg, model.expandoModel)
                }), $elm$core$Platform$Cmd$none);
            case "Resume":
                var _v3 = model.state;
                if (_v3.$ === "Running") return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
                else {
                    var userModel = _v3.c;
                    var userMsg = _v3.d;
                    return _Utils_Tuple2(_Utils_update(model, {
                        expandoModel: A2($elm$browser$Debugger$Expando$merge, userModel, model.expandoModel),
                        expandoMsg: A2($elm$browser$Debugger$Expando$merge, userMsg, model.expandoMsg),
                        state: $elm$browser$Debugger$Main$Running(userModel)
                    }), $elm$browser$Debugger$Main$scroll(model.popout));
                }
            case "Jump":
                var index = msg.a;
                return _Utils_Tuple2(A3($elm$browser$Debugger$Main$jumpUpdate, update, index, model), $elm$core$Platform$Cmd$none);
            case "SliderJump":
                var index = msg.a;
                return _Utils_Tuple2(A3($elm$browser$Debugger$Main$jumpUpdate, update, index, model), A2($elm$browser$Debugger$Main$scrollTo, $elm$browser$Debugger$History$idForMessageIndex(index), model.popout));
            case "Open":
                return _Utils_Tuple2(model, A2($elm$core$Task$perform, $elm$core$Basics$always($elm$browser$Debugger$Main$NoOp), _Debugger_open(model.popout)));
            case "Up":
                var _v4 = model.state;
                if (_v4.$ === "Running") return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
                else {
                    var i = _v4.a;
                    var history1 = _v4.e;
                    var targetIndex = i + 1;
                    if (_Utils_cmp(targetIndex, $elm$browser$Debugger$History$size(history1)) < 0) {
                        var $temp$update = update, $temp$msg = $elm$browser$Debugger$Main$SliderJump(targetIndex), $temp$model = model;
                        update = $temp$update;
                        msg = $temp$msg;
                        model = $temp$model;
                        continue wrapUpdate;
                    } else {
                        var $temp$update = update, $temp$msg = $elm$browser$Debugger$Main$Resume, $temp$model = model;
                        update = $temp$update;
                        msg = $temp$msg;
                        model = $temp$model;
                        continue wrapUpdate;
                    }
                }
            case "Down":
                var _v5 = model.state;
                if (_v5.$ === "Running") {
                    var $temp$update = update, $temp$msg = $elm$browser$Debugger$Main$Jump($elm$browser$Debugger$History$size(model.history) - 1), $temp$model = model;
                    update = $temp$update;
                    msg = $temp$msg;
                    model = $temp$model;
                    continue wrapUpdate;
                } else {
                    var index = _v5.a;
                    if (index > 0) {
                        var $temp$update = update, $temp$msg = $elm$browser$Debugger$Main$SliderJump(index - 1), $temp$model = model;
                        update = $temp$update;
                        msg = $temp$msg;
                        model = $temp$model;
                        continue wrapUpdate;
                    } else return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
                }
            case "Import":
                return A2($elm$browser$Debugger$Main$withGoodMetadata, model, function(_v6) {
                    return _Utils_Tuple2(model, $elm$browser$Debugger$Main$upload(model.popout));
                });
            case "Export":
                return A2($elm$browser$Debugger$Main$withGoodMetadata, model, function(metadata) {
                    return _Utils_Tuple2(model, A2($elm$browser$Debugger$Main$download, metadata, model.history));
                });
            case "Upload":
                var jsonString = msg.a;
                return A2($elm$browser$Debugger$Main$withGoodMetadata, model, function(metadata) {
                    var _v7 = A2($elm$browser$Debugger$Overlay$assessImport, metadata, jsonString);
                    if (_v7.$ === "Err") {
                        var newOverlay = _v7.a;
                        return _Utils_Tuple2(_Utils_update(model, {
                            overlay: newOverlay
                        }), $elm$core$Platform$Cmd$none);
                    } else {
                        var rawHistory = _v7.a;
                        return A3($elm$browser$Debugger$Main$loadNewHistory, rawHistory, update, model);
                    }
                });
            case "OverlayMsg":
                var overlayMsg = msg.a;
                var _v8 = A2($elm$browser$Debugger$Overlay$close, overlayMsg, model.overlay);
                if (_v8.$ === "Nothing") return _Utils_Tuple2(_Utils_update(model, {
                    overlay: $elm$browser$Debugger$Overlay$none
                }), $elm$core$Platform$Cmd$none);
                else {
                    var rawHistory = _v8.a;
                    return A3($elm$browser$Debugger$Main$loadNewHistory, rawHistory, update, model);
                }
            case "SwapLayout":
                return _Utils_Tuple2(_Utils_update(model, {
                    layout: $elm$browser$Debugger$Main$swapLayout(model.layout)
                }), $elm$core$Platform$Cmd$none);
            case "DragStart":
                return _Utils_Tuple2(_Utils_update(model, {
                    layout: A2($elm$browser$Debugger$Main$setDragStatus, $elm$browser$Debugger$Main$Moving, model.layout)
                }), $elm$core$Platform$Cmd$none);
            case "Drag":
                var info = msg.a;
                return _Utils_Tuple2(_Utils_update(model, {
                    layout: A2($elm$browser$Debugger$Main$drag, info, model.layout)
                }), $elm$core$Platform$Cmd$none);
            default:
                return _Utils_Tuple2(_Utils_update(model, {
                    layout: A2($elm$browser$Debugger$Main$setDragStatus, $elm$browser$Debugger$Main$Static, model.layout)
                }), $elm$core$Platform$Cmd$none);
        }
    });
    var $elm$browser$Browser$External = function(a) {
        return {
            $: "External",
            a: a
        };
    };
    var $elm$browser$Browser$Internal = function(a) {
        return {
            $: "Internal",
            a: a
        };
    };
    var $elm$browser$Browser$Dom$NotFound = function(a) {
        return {
            $: "NotFound",
            a: a
        };
    };
    var $elm$url$Url$Http = {
        $: "Http"
    };
    var $elm$url$Url$Https = {
        $: "Https"
    };
    var $elm$url$Url$Url = F6(function(protocol, host, port_, path, query, fragment) {
        return {
            fragment: fragment,
            host: host,
            path: path,
            port_: port_,
            protocol: protocol,
            query: query
        };
    });
    var $elm$core$String$dropLeft = F2(function(n, string) {
        return n < 1 ? string : A3($elm$core$String$slice, n, $elm$core$String$length(string), string);
    });
    var $elm$core$String$indexes = _String_indexes;
    var $elm$core$String$isEmpty = function(string) {
        return string === "";
    };
    var $elm$url$Url$chompBeforePath = F5(function(protocol, path, params, frag, str) {
        if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, "@", str)) return $elm$core$Maybe$Nothing;
        else {
            var _v0 = A2($elm$core$String$indexes, ":", str);
            if (!_v0.b) return $elm$core$Maybe$Just(A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
            else {
                if (!_v0.b.b) {
                    var i = _v0.a;
                    var _v1 = $elm$core$String$toInt(A2($elm$core$String$dropLeft, i + 1, str));
                    if (_v1.$ === "Nothing") return $elm$core$Maybe$Nothing;
                    else {
                        var port_ = _v1;
                        return $elm$core$Maybe$Just(A6($elm$url$Url$Url, protocol, A2($elm$core$String$left, i, str), port_, path, params, frag));
                    }
                } else return $elm$core$Maybe$Nothing;
            }
        }
    });
    var $elm$url$Url$chompBeforeQuery = F4(function(protocol, params, frag, str) {
        if ($elm$core$String$isEmpty(str)) return $elm$core$Maybe$Nothing;
        else {
            var _v0 = A2($elm$core$String$indexes, "/", str);
            if (!_v0.b) return A5($elm$url$Url$chompBeforePath, protocol, "/", params, frag, str);
            else {
                var i = _v0.a;
                return A5($elm$url$Url$chompBeforePath, protocol, A2($elm$core$String$dropLeft, i, str), params, frag, A2($elm$core$String$left, i, str));
            }
        }
    });
    var $elm$url$Url$chompBeforeFragment = F3(function(protocol, frag, str) {
        if ($elm$core$String$isEmpty(str)) return $elm$core$Maybe$Nothing;
        else {
            var _v0 = A2($elm$core$String$indexes, "?", str);
            if (!_v0.b) return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
            else {
                var i = _v0.a;
                return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Just(A2($elm$core$String$dropLeft, i + 1, str)), frag, A2($elm$core$String$left, i, str));
            }
        }
    });
    var $elm$url$Url$chompAfterProtocol = F2(function(protocol, str) {
        if ($elm$core$String$isEmpty(str)) return $elm$core$Maybe$Nothing;
        else {
            var _v0 = A2($elm$core$String$indexes, "#", str);
            if (!_v0.b) return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
            else {
                var i = _v0.a;
                return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Just(A2($elm$core$String$dropLeft, i + 1, str)), A2($elm$core$String$left, i, str));
            }
        }
    });
    var $elm$core$String$startsWith = _String_startsWith;
    var $elm$url$Url$fromString = function(str) {
        return A2($elm$core$String$startsWith, "http://", str) ? A2($elm$url$Url$chompAfterProtocol, $elm$url$Url$Http, A2($elm$core$String$dropLeft, 7, str)) : A2($elm$core$String$startsWith, "https://", str) ? A2($elm$url$Url$chompAfterProtocol, $elm$url$Url$Https, A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing;
    };
    var $elm$core$Basics$never = function(_v0) {
        never: while(true){
            var nvr = _v0.a;
            var $temp$_v0 = nvr;
            _v0 = $temp$_v0;
            continue never;
        }
    };
    var $elm$browser$Browser$document = _Browser_document;
    var $author$project$Main$init = _Utils_Tuple2(_Utils_Tuple0, $elm$core$Platform$Cmd$none);
    var $elm$core$Platform$Sub$batch = _Platform_batch;
    var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
    var $author$project$Main$subscriptions = function(_v0) {
        return $elm$core$Platform$Sub$none;
    };
    var $author$project$Main$update = F2(function(_v0, model) {
        return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
    });
    var $author$project$Neat$Internal$AlignEnd = {
        $: "AlignEnd"
    };
    var $author$project$Neat$Internal$Column = function(a) {
        return {
            $: "Column",
            a: a
        };
    };
    var $author$project$Neat$View$alignBottom = function(_v0) {
        var config = _v0.a;
        return $author$project$Neat$Internal$Column(_Utils_update(config, {
            justify: $author$project$Neat$Internal$AlignEnd
        }));
    };
    var $author$project$Neat$Internal$AlignCenter = {
        $: "AlignCenter"
    };
    var $author$project$Neat$Internal$Row = function(a) {
        return {
            $: "Row",
            a: a
        };
    };
    var $author$project$Neat$View$alignCenter = function(_v0) {
        var config = _v0.a;
        return $author$project$Neat$Internal$Row(_Utils_update(config, {
            justify: $author$project$Neat$Internal$AlignCenter
        }));
    };
    var $author$project$Neat$View$alignRight = function(_v0) {
        var config = _v0.a;
        return $author$project$Neat$Internal$Row(_Utils_update(config, {
            justify: $author$project$Neat$Internal$AlignEnd
        }));
    };
    var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
    var $arowM$elm_mixin$Mixin$Mixin = function(a) {
        return {
            $: "Mixin",
            a: a
        };
    };
    var $arowM$elm_mixin$Mixin$fromAttributes = function(ls) {
        return $arowM$elm_mixin$Mixin$Mixin({
            attributes: ls,
            styles: _List_Nil
        });
    };
    var $arowM$elm_mixin$Mixin$attribute = F2(function(k, v) {
        return $arowM$elm_mixin$Mixin$fromAttributes(_List_fromArray([
            A2($elm$html$Html$Attributes$attribute, k, v)
        ]));
    });
    var $author$project$Neat$Internal$IsGap = function(a) {
        return {
            $: "IsGap",
            a: a
        };
    };
    var $author$project$Neat$customGap = $author$project$Neat$Internal$IsGap;
    var $author$project$Gap$body = $author$project$Neat$customGap({
        horizontal: 0.6,
        vertical: 0.6
    });
    var $arowM$elm_mixin$Mixin$class = function(name) {
        return $arowM$elm_mixin$Mixin$fromAttributes(_List_fromArray([
            $elm$html$Html$Attributes$class(name)
        ]));
    };
    var $author$project$Neat$Internal$Children = F2(function(a, b) {
        return {
            $: "Children",
            a: a,
            b: b
        };
    });
    var $author$project$Neat$Internal$FromColumn = function(a) {
        return {
            $: "FromColumn",
            a: a
        };
    };
    var $author$project$Neat$Internal$None = {
        $: "None"
    };
    var $author$project$Neat$Internal$View = function(a) {
        return {
            $: "View",
            a: a
        };
    };
    var $author$project$Neat$Internal$AlignStart = {
        $: "AlignStart"
    };
    var $arowM$elm_mixin$Mixin$none = $arowM$elm_mixin$Mixin$Mixin({
        attributes: _List_Nil,
        styles: _List_Nil
    });
    var $author$project$Neat$View$defaultColumn_ = F2(function(gap, children) {
        return {
            children: children,
            contentGap: gap,
            justifyContent: $author$project$Neat$Internal$AlignStart,
            mixin: $arowM$elm_mixin$Mixin$none,
            nodeName: "div",
            nominalGap: gap
        };
    });
    var $author$project$Neat$View$emptyGap = {
        horizontal: 0,
        vertical: 0
    };
    var $author$project$Neat$View$extractNominalGap = function(view) {
        switch(view.$){
            case "FromBoundary":
                var g = view.a;
                return g;
            case "FromRow":
                var o = view.a;
                return o.nominalGap;
            case "FromColumn":
                var o = view.a;
                return o.nominalGap;
            case "FromTexts":
                var o = view.a;
                return o.nominalGap;
            default:
                return $author$project$Neat$View$emptyGap;
        }
    };
    var $author$project$Neat$View$column = F2(function(_v0, children_) {
        var justify = _v0.a.justify;
        var children = A2($elm$core$List$filterMap, function(_v2) {
            var item = _v2.a;
            return _Utils_eq(item.content, $author$project$Neat$Internal$None) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(item);
        }, children_);
        return $author$project$Neat$Internal$View(function() {
            if (!children.b) return $author$project$Neat$Internal$None;
            else {
                var item = children.a;
                var items = children.b;
                var itemGap = $author$project$Neat$View$extractNominalGap(item.content);
                var column_ = A2($author$project$Neat$View$defaultColumn_, itemGap, A2($author$project$Neat$Internal$Children, item, items));
                return $author$project$Neat$Internal$FromColumn(_Utils_update(column_, {
                    justifyContent: justify
                }));
            }
        }());
    });
    var $author$project$Neat$Internal$AlignStretch = {
        $: "AlignStretch"
    };
    var $author$project$Neat$Internal$ColumnItem = function(a) {
        return {
            $: "ColumnItem",
            a: a
        };
    };
    var $author$project$Neat$View$columnItem = F2(function(key, _v0) {
        var content = _v0.a;
        return $author$project$Neat$Internal$ColumnItem({
            alignSelf: $author$project$Neat$Internal$AlignStretch,
            content: content,
            grow: false,
            key: key
        });
    });
    var $author$project$Neat$View$defaultColumn = $author$project$Neat$Internal$Column({
        justify: $author$project$Neat$Internal$AlignStart
    });
    var $author$project$Neat$View$defaultRow = $author$project$Neat$Internal$Row({
        justify: $author$project$Neat$Internal$AlignStart,
        wrap: false
    });
    var $author$project$Neat$Internal$Boundary = function(a) {
        return {
            $: "Boundary",
            a: a
        };
    };
    var $author$project$Neat$Internal$ViewContent = function(a) {
        return {
            $: "ViewContent",
            a: a
        };
    };
    var $author$project$Neat$Internal$MaxHeightFit = {
        $: "MaxHeightFit"
    };
    var $author$project$Neat$Internal$MaxWidthFit = {
        $: "MaxWidthFit"
    };
    var $author$project$Neat$Internal$MinHeightInUnit = F2(function(a, b) {
        return {
            $: "MinHeightInUnit",
            a: a,
            b: b
        };
    });
    var $author$project$Neat$Internal$MinSize = {
        $: "MinSize"
    };
    var $author$project$Neat$Internal$MinWidthInUnit = F2(function(a, b) {
        return {
            $: "MinWidthInUnit",
            a: a,
            b: b
        };
    });
    var $author$project$Neat$Internal$NoContent = {
        $: "NoContent"
    };
    var $author$project$Neat$Boundary$emptyGap = {
        horizontal: 0,
        vertical: 0
    };
    var $author$project$Neat$Boundary$defaultBoundary = {
        content: $author$project$Neat$Internal$NoContent,
        enforcePointerEvent: false,
        height: $author$project$Neat$Internal$MinSize,
        horizontalOverflow: false,
        maxHeight: $author$project$Neat$Internal$MaxHeightFit,
        maxWidth: $author$project$Neat$Internal$MaxWidthFit,
        minHeight: A2($author$project$Neat$Internal$MinHeightInUnit, "", 0),
        minWidth: A2($author$project$Neat$Internal$MinWidthInUnit, "", 0),
        mixin: $arowM$elm_mixin$Mixin$none,
        nodeName: "div",
        overlays: _List_Nil,
        padding: $author$project$Neat$Boundary$emptyGap,
        verticalOverflow: false,
        width: $author$project$Neat$Internal$MinSize
    };
    var $author$project$Neat$Boundary$empty = $author$project$Neat$Internal$Boundary(_Utils_update($author$project$Neat$Boundary$defaultBoundary, {
        content: $author$project$Neat$Internal$ViewContent($author$project$Neat$Internal$None)
    }));
    var $author$project$Neat$Boundary$enableVerticalScroll = function(_v0) {
        var boundary = _v0.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            verticalOverflow: true
        }));
    };
    var $author$project$Neat$Internal$FromBoundary = F2(function(a, b) {
        return {
            $: "FromBoundary",
            a: a,
            b: b
        };
    });
    var $author$project$Neat$Internal$FromRow = function(a) {
        return {
            $: "FromRow",
            a: a
        };
    };
    var $author$project$Neat$Internal$FromTexts = function(a) {
        return {
            $: "FromTexts",
            a: a
        };
    };
    var $author$project$Neat$View$expandGap = F2(function(_v0, _v1) {
        var g2 = _v0.a;
        var view = _v1.a;
        return $author$project$Neat$Internal$View(function() {
            switch(view.$){
                case "FromBoundary":
                    var boundary = view.b;
                    return A2($author$project$Neat$Internal$FromBoundary, g2, boundary);
                case "FromRow":
                    var row_ = view.a;
                    return $author$project$Neat$Internal$FromRow(_Utils_update(row_, {
                        nominalGap: g2
                    }));
                case "FromColumn":
                    var column_ = view.a;
                    return $author$project$Neat$Internal$FromColumn(_Utils_update(column_, {
                        nominalGap: g2
                    }));
                case "FromTexts":
                    var texts_ = view.a;
                    return $author$project$Neat$Internal$FromTexts(_Utils_update(texts_, {
                        nominalGap: g2
                    }));
                default:
                    return $author$project$Neat$Internal$None;
            }
        }());
    });
    var $author$project$Neat$Text$none = {
        mixin: $arowM$elm_mixin$Mixin$none,
        nodeName: "span",
        text: ""
    };
    var $author$project$Neat$Text$fromString = function(str) {
        if (str === "") return $author$project$Neat$Text$none;
        else return {
            mixin: $arowM$elm_mixin$Mixin$none,
            nodeName: "span",
            text: str
        };
    };
    var $elm$core$List$filter = F2(function(isGood, list) {
        return A3($elm$core$List$foldr, F2(function(x, xs) {
            return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
        }), _List_Nil, list);
    });
    var $author$project$Neat$View$fromTexts = F2(function(_v0, ls) {
        var gap = _v0.a;
        var texts = A2($elm$core$List$filter, function(a) {
            return a.text !== "";
        }, ls);
        return $author$project$Neat$Internal$View(function() {
            if (!texts.b) return $author$project$Neat$Internal$None;
            else {
                var item = texts.a;
                var items = texts.b;
                return $author$project$Neat$Internal$FromTexts({
                    contentGap: gap,
                    mixin: $arowM$elm_mixin$Mixin$none,
                    nodeName: "div",
                    nominalGap: gap,
                    texts: _Utils_Tuple2(item, items)
                });
            }
        }());
    });
    var $author$project$Neat$View$grownCenterItem = F2(function(key, _v0) {
        var content = _v0.a;
        return $author$project$Neat$Internal$ColumnItem({
            alignSelf: $author$project$Neat$Internal$AlignCenter,
            content: content,
            grow: true,
            key: key
        });
    });
    var $author$project$Neat$View$grownColumnItem = F2(function(key, _v0) {
        var content = _v0.a;
        return $author$project$Neat$Internal$ColumnItem({
            alignSelf: $author$project$Neat$Internal$AlignStretch,
            content: content,
            grow: true,
            key: key
        });
    });
    var $author$project$Neat$View$grownLeftItem = F2(function(key, _v0) {
        var content = _v0.a;
        return $author$project$Neat$Internal$ColumnItem({
            alignSelf: $author$project$Neat$Internal$AlignStart,
            content: content,
            grow: true,
            key: key
        });
    });
    var $author$project$Neat$Internal$RowItem = function(a) {
        return {
            $: "RowItem",
            a: a
        };
    };
    var $author$project$Neat$View$grownMiddleItem = F2(function(key, _v0) {
        var content = _v0.a;
        return $author$project$Neat$Internal$RowItem({
            alignSelf: $author$project$Neat$Internal$AlignCenter,
            content: content,
            grow: true,
            key: key
        });
    });
    var $author$project$Neat$View$grownRowItem = F2(function(key, _v0) {
        var content = _v0.a;
        return $author$project$Neat$Internal$RowItem({
            alignSelf: $author$project$Neat$Internal$AlignStretch,
            content: content,
            grow: true,
            key: key
        });
    });
    var $author$project$Neat$View$middleItem = F2(function(key, _v0) {
        var content = _v0.a;
        return $author$project$Neat$Internal$RowItem({
            alignSelf: $author$project$Neat$Internal$AlignCenter,
            content: content,
            grow: false,
            key: key
        });
    });
    var $author$project$Neat$emptyGap = {
        horizontal: 0,
        vertical: 0
    };
    var $author$project$Neat$noGap = $author$project$Neat$Internal$IsGap($author$project$Neat$emptyGap);
    var $author$project$Neat$Internal$HtmlContent = function(a) {
        return {
            $: "HtmlContent",
            a: a
        };
    };
    var $author$project$Neat$Internal$StringContent = function(a) {
        return {
            $: "StringContent",
            a: a
        };
    };
    var $author$project$Neat$Internal$TextsContent = function(a) {
        return {
            $: "TextsContent",
            a: a
        };
    };
    var $elm$virtual_dom$VirtualDom$mapAttribute = _VirtualDom_mapAttribute;
    var $elm$html$Html$Attributes$map = $elm$virtual_dom$VirtualDom$mapAttribute;
    var $arowM$elm_mixin$Mixin$map = F2(function(f, _v0) {
        var mixin = _v0.a;
        return $arowM$elm_mixin$Mixin$Mixin({
            attributes: A2($elm$core$List$map, $elm$html$Html$Attributes$map(f), mixin.attributes),
            styles: mixin.styles
        });
    });
    var $author$project$Neat$Text$map = F2(function(f, text) {
        return {
            mixin: A2($arowM$elm_mixin$Mixin$map, f, text.mixin),
            nodeName: text.nodeName,
            text: text.text
        };
    });
    var $author$project$Neat$Boundary$modifyChild = F2(function(f, _v0) {
        var item0 = _v0.a;
        var items = _v0.b;
        var modifyContent = function(item) {
            return {
                alignSelf: item.alignSelf,
                content: f(item.content),
                grow: item.grow,
                key: item.key
            };
        };
        return A2($author$project$Neat$Internal$Children, modifyContent(item0), A2($elm$core$List$map, modifyContent, items));
    });
    var $author$project$Neat$Boundary$mapBoundary = F2(function(f, _v4) {
        var boundary = _v4.a;
        return $author$project$Neat$Internal$Boundary(A2($author$project$Neat$Boundary$mapBoundary_, f, boundary));
    });
    var $author$project$Neat$Boundary$mapBoundary_ = F2(function(f, o) {
        return {
            content: function() {
                var _v2 = o.content;
                switch(_v2.$){
                    case "TextsContent":
                        var texts = _v2.a;
                        return $author$project$Neat$Internal$TextsContent(A2($elm$core$List$map, $author$project$Neat$Text$map(f), texts));
                    case "ViewContent":
                        var view = _v2.a;
                        return $author$project$Neat$Internal$ViewContent(A2($author$project$Neat$Boundary$mapView_, f, view));
                    case "HtmlContent":
                        var children = _v2.a;
                        return $author$project$Neat$Internal$HtmlContent(A2($elm$core$List$map, function(_v3) {
                            var k = _v3.a;
                            var b = _v3.b;
                            return _Utils_Tuple2(k, A2($author$project$Neat$Boundary$mapBoundary_, f, b));
                        }, children));
                    case "StringContent":
                        var str = _v2.a;
                        return $author$project$Neat$Internal$StringContent(str);
                    default:
                        return $author$project$Neat$Internal$NoContent;
                }
            }(),
            enforcePointerEvent: o.enforcePointerEvent,
            height: o.height,
            horizontalOverflow: o.horizontalOverflow,
            maxHeight: o.maxHeight,
            maxWidth: o.maxWidth,
            minHeight: o.minHeight,
            minWidth: o.minWidth,
            mixin: A2($arowM$elm_mixin$Mixin$map, f, o.mixin),
            nodeName: o.nodeName,
            overlays: A2($author$project$Neat$Boundary$mapOverlays, f, o.overlays),
            padding: o.padding,
            verticalOverflow: o.verticalOverflow,
            width: o.width
        };
    });
    var $author$project$Neat$Boundary$mapOverlays = function(f) {
        return $elm$core$List$map(function(o) {
            return {
                area: o.area,
                boundary: A2($author$project$Neat$Boundary$mapBoundary, f, o.boundary),
                name: o.name
            };
        });
    };
    var $author$project$Neat$Boundary$mapView_ = F2(function(f, view) {
        switch(view.$){
            case "FromBoundary":
                var g = view.a;
                var boundary = view.b;
                return A2($author$project$Neat$Internal$FromBoundary, g, A2($author$project$Neat$Boundary$mapBoundary_, f, boundary));
            case "FromRow":
                var o = view.a;
                return $author$project$Neat$Internal$FromRow({
                    children: A2($author$project$Neat$Boundary$modifyChild, $author$project$Neat$Boundary$mapView_(f), o.children),
                    contentGap: o.contentGap,
                    justifyContent: o.justifyContent,
                    mixin: A2($arowM$elm_mixin$Mixin$map, f, o.mixin),
                    nodeName: o.nodeName,
                    nominalGap: o.nominalGap,
                    wrap: o.wrap
                });
            case "FromColumn":
                var o = view.a;
                return $author$project$Neat$Internal$FromColumn({
                    children: A2($author$project$Neat$Boundary$modifyChild, $author$project$Neat$Boundary$mapView_(f), o.children),
                    contentGap: o.contentGap,
                    justifyContent: o.justifyContent,
                    mixin: A2($arowM$elm_mixin$Mixin$map, f, o.mixin),
                    nodeName: o.nodeName,
                    nominalGap: o.nominalGap
                });
            case "FromTexts":
                var o = view.a;
                return $author$project$Neat$Internal$FromTexts({
                    contentGap: o.contentGap,
                    mixin: A2($arowM$elm_mixin$Mixin$map, f, o.mixin),
                    nodeName: o.nodeName,
                    nominalGap: o.nominalGap,
                    texts: function() {
                        var _v1 = o.texts;
                        var head = _v1.a;
                        var tail = _v1.b;
                        return _Utils_Tuple2(A2($author$project$Neat$Text$map, f, head), A2($elm$core$List$map, $author$project$Neat$Text$map(f), tail));
                    }()
                });
            default:
                return $author$project$Neat$Internal$None;
        }
    });
    var $author$project$Neat$Boundary$putLayer = F3(function(name, _v0, _v1) {
        var area = _v0.a;
        var layered = _v0.b;
        var boundary = _v1.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            overlays: A2($elm$core$List$cons, {
                area: area,
                boundary: A2($author$project$Neat$Boundary$mapBoundary, function(_v2) {
                    var a = _v2.a;
                    return a;
                }, layered),
                name: name
            }, boundary.overlays)
        }));
    });
    var $author$project$Neat$View$defaultRow_ = F2(function(gap, children) {
        return {
            children: children,
            contentGap: gap,
            justifyContent: $author$project$Neat$Internal$AlignStart,
            mixin: $arowM$elm_mixin$Mixin$none,
            nodeName: "div",
            nominalGap: gap,
            wrap: true
        };
    });
    var $author$project$Neat$View$row = F2(function(_v0, children_) {
        var justify = _v0.a.justify;
        var wrap = _v0.a.wrap;
        var children = A2($elm$core$List$filterMap, function(_v2) {
            var item = _v2.a;
            return _Utils_eq(item.content, $author$project$Neat$Internal$None) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(item);
        }, children_);
        return $author$project$Neat$Internal$View(function() {
            if (!children.b) return $author$project$Neat$Internal$None;
            else {
                var item = children.a;
                var items = children.b;
                var itemGap = $author$project$Neat$View$extractNominalGap(item.content);
                var row_ = A2($author$project$Neat$View$defaultRow_, itemGap, A2($author$project$Neat$Internal$Children, item, items));
                return $author$project$Neat$Internal$FromRow(_Utils_update(row_, {
                    justifyContent: justify,
                    wrap: wrap
                }));
            }
        }());
    });
    var $author$project$Neat$View$defaultBoundary = {
        content: $author$project$Neat$Internal$NoContent,
        enforcePointerEvent: false,
        height: $author$project$Neat$Internal$MinSize,
        horizontalOverflow: false,
        maxHeight: $author$project$Neat$Internal$MaxHeightFit,
        maxWidth: $author$project$Neat$Internal$MaxWidthFit,
        minHeight: A2($author$project$Neat$Internal$MinHeightInUnit, "", 0),
        minWidth: A2($author$project$Neat$Internal$MinWidthInUnit, "", 0),
        mixin: $arowM$elm_mixin$Mixin$none,
        nodeName: "div",
        overlays: _List_Nil,
        padding: $author$project$Neat$View$emptyGap,
        verticalOverflow: false,
        width: $author$project$Neat$Internal$MinSize
    };
    var $author$project$Neat$View$setBoundary = function(_v0) {
        var view = _v0.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update($author$project$Neat$View$defaultBoundary, {
            content: _Utils_eq(view, $author$project$Neat$Internal$None) ? $author$project$Neat$Internal$NoContent : $author$project$Neat$Internal$ViewContent(view),
            padding: $author$project$Neat$View$extractNominalGap(view)
        }));
    };
    var $arowM$elm_mixin$Mixin$mempty = $arowM$elm_mixin$Mixin$Mixin({
        attributes: _List_Nil,
        styles: _List_Nil
    });
    var $arowM$elm_mixin$Mixin$batch = A2($elm$core$List$foldl, F2(function(_v0, _v1) {
        var a = _v0.a;
        var acc = _v1.a;
        return $arowM$elm_mixin$Mixin$Mixin({
            attributes: _Utils_ap(a.attributes, acc.attributes),
            styles: _Utils_ap(a.styles, acc.styles)
        });
    }), $arowM$elm_mixin$Mixin$mempty);
    var $author$project$Neat$Text$setMixin = F2(function(mixin, node) {
        return _Utils_update(node, {
            mixin: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                node.mixin,
                mixin
            ]))
        });
    });
    var $author$project$Neat$Text$setClass = A2($elm$core$Basics$composeL, $author$project$Neat$Text$setMixin, $arowM$elm_mixin$Mixin$class);
    var $author$project$Neat$Boundary$setGap = F2(function(_v0, _v1) {
        var gap = _v0.a;
        var boundary = _v1.a;
        return $author$project$Neat$Internal$View(function() {
            var _v2 = boundary.content;
            if (_v2.$ === "NoContent") return $author$project$Neat$Internal$None;
            else return A2($author$project$Neat$Internal$FromBoundary, gap, boundary);
        }());
    });
    var $author$project$Neat$Internal$MaxHeightInUnit = F2(function(a, b) {
        return {
            $: "MaxHeightInUnit",
            a: a,
            b: b
        };
    });
    var $author$project$Neat$Internal$FlexSize = {
        $: "FlexSize"
    };
    var $author$project$Neat$Boundary$setMaxHeight = F2(function(length, _v0) {
        var boundary = _v0.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            height: $author$project$Neat$Internal$FlexSize,
            maxHeight: length
        }));
    });
    var $author$project$Neat$Boundary$setMaxHeightInEm = A2($elm$core$Basics$composeL, $author$project$Neat$Boundary$setMaxHeight, $author$project$Neat$Internal$MaxHeightInUnit("em"));
    var $author$project$Neat$Internal$MaxWidthInUnit = F2(function(a, b) {
        return {
            $: "MaxWidthInUnit",
            a: a,
            b: b
        };
    });
    var $author$project$Neat$Boundary$setMaxWidth = F2(function(length, _v0) {
        var boundary = _v0.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            maxWidth: length,
            width: $author$project$Neat$Internal$FlexSize
        }));
    });
    var $author$project$Neat$Boundary$setMaxWidthInEm = A2($elm$core$Basics$composeL, $author$project$Neat$Boundary$setMaxWidth, $author$project$Neat$Internal$MaxWidthInUnit("em"));
    var $author$project$Neat$Boundary$setMinHeight = F2(function(length, _v0) {
        var boundary = _v0.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            minHeight: length
        }));
    });
    var $author$project$Neat$Boundary$setMinHeightInEm = A2($elm$core$Basics$composeL, $author$project$Neat$Boundary$setMinHeight, $author$project$Neat$Internal$MinHeightInUnit("em"));
    var $author$project$Neat$Boundary$setMinWidth = F2(function(length, _v0) {
        var boundary = _v0.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            minWidth: length
        }));
    });
    var $author$project$Neat$Boundary$setMinWidthInEm = A2($elm$core$Basics$composeL, $author$project$Neat$Boundary$setMinWidth, $author$project$Neat$Internal$MinWidthInUnit("em"));
    var $author$project$Neat$Boundary$setMixin = F2(function(_new, _v0) {
        var boundary = _v0.a;
        return $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            mixin: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                boundary.mixin,
                _new
            ]))
        }));
    });
    var $author$project$Neat$Text$setNodeName = F2(function(str, text) {
        return _Utils_update(text, {
            nodeName: str
        });
    });
    var $author$project$Neat$View$setNodeName = F2(function(str, _v0) {
        var view = _v0.a;
        return $author$project$Neat$Internal$View(function() {
            switch(view.$){
                case "FromBoundary":
                    var g = view.a;
                    var boundary = view.b;
                    return A2($author$project$Neat$Internal$FromBoundary, g, _Utils_update(boundary, {
                        nodeName: str
                    }));
                case "FromRow":
                    var row_ = view.a;
                    return $author$project$Neat$Internal$FromRow(_Utils_update(row_, {
                        nodeName: str
                    }));
                case "FromColumn":
                    var column_ = view.a;
                    return $author$project$Neat$Internal$FromColumn(_Utils_update(column_, {
                        nodeName: str
                    }));
                case "FromTexts":
                    var texts_ = view.a;
                    return $author$project$Neat$Internal$FromTexts(_Utils_update(texts_, {
                        nodeName: str
                    }));
                default:
                    return $author$project$Neat$Internal$None;
            }
        }());
    });
    var $arowM$elm_mixin$Mixin$style = F2(function(key, val) {
        return $arowM$elm_mixin$Mixin$Mixin({
            attributes: _List_Nil,
            styles: _List_fromArray([
                _Utils_Tuple2(key, val)
            ])
        });
    });
    var $author$project$Gap$sub = $author$project$Neat$customGap({
        horizontal: 0.3,
        vertical: 0.3
    });
    var $author$project$Neat$View$textBlock = F2(function(gap, str) {
        return A2($author$project$Neat$View$fromTexts, gap, _List_fromArray([
            $author$project$Neat$Text$fromString(str)
        ]));
    });
    var $author$project$Neat$Internal$Layered = function(a) {
        return {
            $: "Layered",
            a: a
        };
    };
    var $author$project$Neat$Boundary$toLayered = function(_v0) {
        var boundary = _v0.a;
        return A2($author$project$Neat$Boundary$mapBoundary, $author$project$Neat$Internal$Layered, $author$project$Neat$Internal$Boundary(_Utils_update(boundary, {
            enforcePointerEvent: true
        })));
    };
    var $author$project$Main$body = function(_v0) {
        return $author$project$Neat$View$setBoundary(A2($author$project$Neat$View$column, $author$project$Neat$View$alignBottom($author$project$Neat$View$defaultColumn), _List_fromArray([
            A2($author$project$Neat$View$columnItem, "header", A2($author$project$Neat$Boundary$setGap, $author$project$Neat$noGap, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("header"), $author$project$Neat$View$setBoundary(A2($author$project$Neat$View$row, $author$project$Neat$View$defaultRow, _List_fromArray([
                A2($author$project$Neat$View$grownMiddleItem, "text", A2($author$project$Neat$View$textBlock, $author$project$Gap$body, "Header")),
                A2($author$project$Neat$View$grownMiddleItem, "input", A2($author$project$Neat$View$setNodeName, "input", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$body, A2($author$project$Neat$Boundary$setMixin, A2($arowM$elm_mixin$Mixin$style, "padding", "0.4em"), A2($author$project$Neat$Boundary$setMixin, A2($arowM$elm_mixin$Mixin$attribute, "placeholder", "foo"), A2($author$project$Neat$Boundary$setMinWidthInEm, 4, $author$project$Neat$Boundary$empty)))))),
                A2($author$project$Neat$View$middleItem, "hamburger", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$body, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("header_hamburger"), A2($author$project$Neat$Boundary$setMaxHeightInEm, 3, A2($author$project$Neat$Boundary$setMinHeightInEm, 3, A2($author$project$Neat$Boundary$setMaxWidthInEm, 3, A2($author$project$Neat$Boundary$setMinWidthInEm, 3, $author$project$Neat$View$setBoundary(A2($author$project$Neat$View$row, $author$project$Neat$View$alignCenter($author$project$Neat$View$defaultRow), _List_fromArray([
                    A2($author$project$Neat$View$middleItem, "icon", A2($author$project$Neat$View$textBlock, $author$project$Neat$noGap, "三"))
                ]))))))))))
            ])))))),
            A2($author$project$Neat$View$grownColumnItem, "body", A2($author$project$Neat$Boundary$setGap, $author$project$Neat$noGap, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("blue"), $author$project$Neat$Boundary$enableVerticalScroll(A2($author$project$Neat$Boundary$setMinHeightInEm, 10, $author$project$Neat$View$setBoundary(A2($author$project$Neat$View$column, $author$project$Neat$View$defaultColumn, _List_fromArray([
                A2($author$project$Neat$View$columnItem, "sampleText", A2($author$project$Neat$View$expandGap, $author$project$Gap$body, A2($author$project$Neat$View$fromTexts, $author$project$Gap$sub, _List_fromArray([
                    $author$project$Neat$Text$fromString("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do "),
                    A2($author$project$Neat$Text$setNodeName, "code", A2($author$project$Neat$Text$setClass, "inlineCode", $author$project$Neat$Text$fromString("<eiusmod>"))),
                    $author$project$Neat$Text$fromString(" tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                ])))),
                A2($author$project$Neat$View$columnItem, "subBoxes", A2($author$project$Neat$View$expandGap, $author$project$Gap$body, A2($author$project$Neat$View$column, $author$project$Neat$View$defaultColumn, _List_fromArray([
                    A2($author$project$Neat$View$columnItem, "sampleBox1", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$sub, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("red"), A2($author$project$Neat$Boundary$setMaxWidthInEm, 20, A2($author$project$Neat$Boundary$setMinWidthInEm, 10, A2($author$project$Neat$Boundary$setMinHeightInEm, 2, $author$project$Neat$Boundary$empty)))))),
                    A2($author$project$Neat$View$columnItem, "sampleBox2", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$sub, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("blue"), A2($author$project$Neat$Boundary$setMinWidthInEm, 7, A2($author$project$Neat$Boundary$setMinHeightInEm, 2, $author$project$Neat$Boundary$empty)))))
                ])))),
                A2($author$project$Neat$View$grownCenterItem, "sampleNestedBox", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$body, A3($author$project$Neat$Boundary$putLayer, "overlay", _Utils_Tuple2({
                    bottom: 0,
                    left: 0,
                    priority: $elm$core$Maybe$Nothing,
                    right: 50,
                    top: 50
                }, $author$project$Neat$Boundary$toLayered(A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("red"), A2($author$project$Neat$Boundary$setMaxWidthInEm, 20, $author$project$Neat$Boundary$empty)))), A2($author$project$Neat$Boundary$setMaxHeightInEm, 33, A2($author$project$Neat$Boundary$setMaxWidthInEm, 40, A2($author$project$Neat$Boundary$setMinWidthInEm, 37, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("red"), $author$project$Neat$View$setBoundary(A2($author$project$Neat$View$row, $author$project$Neat$View$alignRight($author$project$Neat$View$defaultRow), _List_fromArray([
                    A2($author$project$Neat$View$grownRowItem, "sampleBox", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$body, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("blue"), A2($author$project$Neat$Boundary$setMaxWidthInEm, 23, A2($author$project$Neat$Boundary$setMinWidthInEm, 12, A2($author$project$Neat$Boundary$setMinHeightInEm, 5, $author$project$Neat$Boundary$empty))))))
                ])))))))))),
                A2($author$project$Neat$View$grownLeftItem, "content1", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$body, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("scrollableText_content"), A2($author$project$Neat$Boundary$setMaxHeightInEm, 8, A2($author$project$Neat$Boundary$setMinHeightInEm, 1, $author$project$Neat$Boundary$enableVerticalScroll($author$project$Neat$View$setBoundary(A2($author$project$Neat$View$row, $author$project$Neat$View$defaultRow, _List_fromArray([
                    A2($author$project$Neat$View$grownRowItem, "content", A2($author$project$Neat$View$textBlock, $author$project$Gap$sub, "foo000000\nbar\nbar\nbar\nfoo\nbar\nbar\nbar\n"))
                ]))))))))),
                A2($author$project$Neat$View$grownColumnItem, "content2", A2($author$project$Neat$Boundary$setGap, $author$project$Gap$body, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("scrollableText_content"), A2($author$project$Neat$Boundary$setMaxHeightInEm, 8, A2($author$project$Neat$Boundary$setMinHeightInEm, 1, $author$project$Neat$Boundary$enableVerticalScroll($author$project$Neat$View$setBoundary(A2($author$project$Neat$View$expandGap, $author$project$Gap$body, A2($author$project$Neat$View$row, $author$project$Neat$View$defaultRow, _List_fromArray([
                    A2($author$project$Neat$View$grownRowItem, "content", A2($author$project$Neat$View$textBlock, $author$project$Gap$sub, "foo000000\nbar\nbar\nbar"))
                ]))))))))))
            ])))))))),
            A2($author$project$Neat$View$columnItem, "footer", A2($author$project$Neat$Boundary$setGap, $author$project$Neat$noGap, A2($author$project$Neat$Boundary$setMixin, $arowM$elm_mixin$Mixin$class("footer"), $author$project$Neat$View$setBoundary(A2($author$project$Neat$View$textBlock, $author$project$Gap$body, "Footer")))))
        ])));
    };
    var $author$project$Neat$Internal$Renderer = function(a) {
        return {
            $: "Renderer",
            a: a
        };
    };
    var $author$project$Neat$Internal$BaseSize = F2(function(a, b) {
        return {
            $: "BaseSize",
            a: a,
            b: b
        };
    });
    var $author$project$Neat$defaultRenderer_ = {
        baseSize: A2($author$project$Neat$Internal$BaseSize, "rem", 1),
        debug: false
    };
    var $author$project$Neat$defaultRenderer = $author$project$Neat$Internal$Renderer($author$project$Neat$defaultRenderer_);
    var $author$project$Neat$class = function(str) {
        return $arowM$elm_mixin$Mixin$class("elmNeatLayout--" + str);
    };
    var $arowM$elm_mixin$Mixin$toAttributes = function(_v0) {
        var mixin = _v0.a;
        return function(a) {
            return A2($elm$core$List$cons, a, mixin.attributes);
        }(A2($elm$html$Html$Attributes$attribute, "style", A3($elm$core$List$foldl, F2(function(_v1, acc) {
            var k = _v1.a;
            var v = _v1.b;
            return acc + (k + (":" + (v + ";")));
        }), "", mixin.styles)));
    };
    var $arowM$elm_mixin$Mixin$lift = F2(function(f, mixins) {
        return f($arowM$elm_mixin$Mixin$toAttributes($arowM$elm_mixin$Mixin$batch(mixins)));
    });
    var $arowM$elm_mixin$Mixin$Html$div = $arowM$elm_mixin$Mixin$lift($elm$html$Html$div);
    var $author$project$Neat$neatLayoutStyle = '.elmNeatLayout,.elmNeatLayout:before,.elmNeatLayout:after{box-sizing:border-box;margin:0;padding:0}.elmNeatLayout--top{display:block;position:fixed;inset:0;overflow:hidden}.elmNeatLayout--overlay{pointer-events:none;top:var(--overlay-top);bottom:var(--overlay-bottom);left:var(--overlay-left);right:var(--overlay-right);z-index:var(--overlay-priority);display:block;position:absolute;overflow:hidden}.elmNeatLayout--boundary{overflow:hidden}.elmNeatLayout--boundary-hasOverlays:not(.elmNeatLayout--top){position:relative}.elmNeatLayout--boundary-enforcePointerEvent{pointer-events:auto}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller{height:100%;width:100%}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller-verticalScroll>.elmNeatLayout--boundaryContent{height:auto;min-height:100%}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller-horizontalScroll>.elmNeatLayout--boundaryContent{width:auto;min-width:100%}.elmNeatLayout--boundary-view-hasContent,.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller{padding:var(--inner-gap-y)var(--inner-gap-x)}.elmNeatLayout--boundary-text{overflow:visible}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin{height:auto;width:100%}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin:before{width:0;height:0;margin-top:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin:after{width:0;height:0;margin-bottom:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin>.elmNeatLayout--boundary_text{line-height:calc(1em + var(--outer-gap-y));display:inline}.elmNeatLayout--boundary-horizontalOverflow,.elmNeatLayout--boundary-horizontalOverflow>.elmNeatLayout--boundary_scroller{overflow-x:auto}.elmNeatLayout--boundary-verticalOverflow{overflow-y:auto}.elmNeatLayout--boundary-verticalOverflow-wrapper{overflow-y:hidden}.elmNeatLayout--boundary-verticalOverflow>.elmNeatLayout--boundary_scroller{overflow-y:auto}.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin{overflow-y:hidden}.elmNeatLayout--boundary-verticalOverflow>.elmNeatLayout--boundaryContent.elmNeatLayout--column{height:auto}.elmNeatLayout--boundary-hasMinHeight{min-height:var(--min-height)}.elmNeatLayout--boundary-hasMaxHeight:not(.elmNeatLayout--boundary-verticalOverflow){max-height:var(--max-height)}.elmNeatLayout--boundary-hasMinWidth{min-width:var(--min-width)}.elmNeatLayout--boundary-hasMaxWidth:not(.elmNeatLayout--boundary-horizontalOverflow){max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--rowChild{flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild:not(.elmNeatLayout--boundary-horizontalOverflow){width:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex:not(.elmNeatLayout--boundary-verticalOverflow){height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex:not(.elmNeatLayout--boundary-verticalOverflow).elmNeatLayout--rowChild-alignStretch,.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex.elmNeatLayout--boundary-hasMaxHeight{max-height:var(--max-height)}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightMinSize{height:auto;max-height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightMinSize.elmNeatLayout--boundary-hasMaxHeight{max-height:min(var(--max-height),100%)}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-horizontalOverflow{width:0;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--boundary-hasMaxWidth{max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--columnChild{flex-shrink:0}.elmNeatLayout--boundary.elmNeatLayout--columnChild:not(.elmNeatLayout--boundary-verticalOverflow){height:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex:not(.elmNeatLayout--boundary-horizontalOverflow){width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex:not(.elmNeatLayout--boundary-horizontalOverflow).elmNeatLayout--columnChild-alignStretch,.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex.elmNeatLayout--boundary-hasMaxWidth{max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthMinSize{width:auto;max-width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthMinSize.elmNeatLayout--boundary-hasMaxWidth{max-width:min(var(--max-width),100%)}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-verticalOverflow{height:0;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-hasMaxHeight{max-height:var(--max-height)}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-verticalOverflow{height:auto;max-height:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-hasMaxHeight{max-height:min(var(--max-height),100%)}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-horizontalOverflow{width:auto;max-width:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--boundary-hasMaxWidth{max-width:min(var(--max-width),100%)}.elmNeatLayout--row{gap:var(--content-gap-y)var(--content-gap-x);flex-flow:row;display:flex}.elmNeatLayout--row.elmNeatLayout--row-wrap{flex-wrap:wrap}.elmNeatLayout--row.elmNeatLayout--row-justifyStart{justify-content:flex-start}.elmNeatLayout--row.elmNeatLayout--row-justifyCenter{justify-content:center}.elmNeatLayout--row.elmNeatLayout--row-justifyEnd{justify-content:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild{flex-grow:0}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-grow{flex-grow:1}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStart{align-self:flex-start}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignCenter{align-self:center}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignEnd{align-self:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{align-self:stretch}.elmNeatLayout--row.elmNeatLayout--rowChild{width:auto;height:auto;flex-shrink:1}.elmNeatLayout--row.elmNeatLayout--columnChild{height:auto;width:100%;flex-shrink:0}.elmNeatLayout--row.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--row.elmNeatLayout--boundaryContent{width:100%;height:100%}.elmNeatLayout--column{gap:var(--content-gap-y)var(--content-gap-x);flex-flow:column;display:flex}.elmNeatLayout--column.elmNeatLayout--column-justifyStart{justify-content:flex-start}.elmNeatLayout--column.elmNeatLayout--column-justifyCenter{justify-content:center}.elmNeatLayout--column.elmNeatLayout--column-justifyEnd{justify-content:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild{flex-grow:0}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-grow{flex-grow:1}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStart{align-self:flex-start}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignCenter{align-self:center}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignEnd{align-self:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{align-self:stretch}.elmNeatLayout--column.elmNeatLayout--rowChild{width:auto;height:100%;flex-shrink:1}.elmNeatLayout--column.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--column.elmNeatLayout--columnChild{height:auto;width:auto;flex-shrink:0}.elmNeatLayout--column.elmNeatLayout--boundaryContent{width:100%;height:100%}.elmNeatLayout--textView{overflow:visible}.elmNeatLayout--textView>.elmNeatLayout--textView_textMargin{height:auto;width:100%}.elmNeatLayout--textView>.elmNeatLayout--textView_textMargin:before{width:0;height:0;margin-top:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--textView>.elmNeatLayout--textView_textMargin:after{width:0;height:0;margin-bottom:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--textView>.elmNeatLayout--textView_textMargin>.elmNeatLayout--textView_inline{line-height:calc(1em + var(--content-gap-y));display:inline}';
    var $arowM$elm_mixin$Mixin$Html$node = function(name) {
        return $arowM$elm_mixin$Mixin$lift($elm$html$Html$node(name));
    };
    var $author$project$Neat$mapBaseSize = F2(function(f, _v0) {
        var unit = _v0.a;
        var a = _v0.b;
        return A2($author$project$Neat$Internal$BaseSize, unit, f(a));
    });
    var $author$project$Neat$multipleBaseSize = function(a) {
        return $author$project$Neat$mapBaseSize(function(s) {
            return a * s;
        });
    };
    var $elm$core$String$concat = function(strings) {
        return A2($elm$core$String$join, "", strings);
    };
    var $author$project$Neat$renderBaseSize = function(_v0) {
        var unit = _v0.a;
        var a = _v0.b;
        return $elm$core$String$concat(_List_fromArray([
            $elm$core$String$fromFloat(a),
            unit
        ]));
    };
    var $author$project$Neat$boundaryCustomProperty = F3(function(renderer, _v0, o) {
        var outerGap = _v0.outerGap;
        return $arowM$elm_mixin$Mixin$batch(_List_fromArray([
            A2($arowM$elm_mixin$Mixin$style, "--outer-gap-x", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, outerGap.horizontal, renderer.baseSize))),
            A2($arowM$elm_mixin$Mixin$style, "--outer-gap-y", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, outerGap.vertical, renderer.baseSize))),
            A2($arowM$elm_mixin$Mixin$style, "--inner-gap-x", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.padding.horizontal, renderer.baseSize))),
            A2($arowM$elm_mixin$Mixin$style, "--inner-gap-y", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.padding.vertical, renderer.baseSize))),
            function() {
                var _v1 = o.minWidth;
                if (_v1.$ === "MinWidthInBs") {
                    var bs = _v1.a;
                    return A2($arowM$elm_mixin$Mixin$style, "--min-width", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, bs, renderer.baseSize)));
                } else {
                    var unit = _v1.a;
                    var v = _v1.b;
                    return A2($arowM$elm_mixin$Mixin$style, "--min-width", _Utils_ap($elm$core$String$fromFloat(v), unit));
                }
            }(),
            function() {
                var _v2 = o.maxWidth;
                switch(_v2.$){
                    case "MaxWidthInBs":
                        var bs = _v2.a;
                        return A2($arowM$elm_mixin$Mixin$style, "--max-width", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, bs, renderer.baseSize)));
                    case "MaxWidthInUnit":
                        var unit = _v2.a;
                        var v = _v2.b;
                        return A2($arowM$elm_mixin$Mixin$style, "--max-width", _Utils_ap($elm$core$String$fromFloat(v), unit));
                    default:
                        return $arowM$elm_mixin$Mixin$none;
                }
            }(),
            function() {
                var _v3 = o.minHeight;
                if (_v3.$ === "MinHeightInBs") {
                    var bs = _v3.a;
                    return A2($arowM$elm_mixin$Mixin$style, "--min-height", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, bs, renderer.baseSize)));
                } else {
                    var unit = _v3.a;
                    var v = _v3.b;
                    return A2($arowM$elm_mixin$Mixin$style, "--min-height", _Utils_ap($elm$core$String$fromFloat(v), unit));
                }
            }(),
            function() {
                var _v4 = o.maxHeight;
                switch(_v4.$){
                    case "MaxHeightInBs":
                        var bs = _v4.a;
                        return A2($arowM$elm_mixin$Mixin$style, "--max-height", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, bs, renderer.baseSize)));
                    case "MaxHeightInUnit":
                        var unit = _v4.a;
                        var v = _v4.b;
                        return A2($arowM$elm_mixin$Mixin$style, "--max-height", _Utils_ap($elm$core$String$fromFloat(v), unit));
                    default:
                        return $arowM$elm_mixin$Mixin$none;
                }
            }()
        ]));
    });
    var $author$project$Neat$hasMaxHeight = function(mh) {
        switch(mh.$){
            case "MaxHeightInBs":
                return true;
            case "MaxHeightInUnit":
                return true;
            default:
                return false;
        }
    };
    var $author$project$Neat$hasMaxWidth = function(mw) {
        switch(mw.$){
            case "MaxWidthInBs":
                return true;
            case "MaxWidthInUnit":
                return true;
            default:
                return false;
        }
    };
    var $arowM$elm_mixin$Mixin$Html$keyed = function(name) {
        return $arowM$elm_mixin$Mixin$lift($elm$html$Html$Keyed$node(name));
    };
    var $elm$core$Maybe$map = F2(function(f, maybe) {
        if (maybe.$ === "Just") {
            var value = maybe.a;
            return $elm$core$Maybe$Just(f(value));
        } else return $elm$core$Maybe$Nothing;
    });
    var $author$project$Neat$minHeightZero = function(minHeight) {
        if (minHeight.$ === "MinHeightInBs") {
            var a = minHeight.a;
            return !a;
        } else {
            var a = minHeight.b;
            return !a;
        }
    };
    var $author$project$Neat$minWidthZero = function(minWidth) {
        if (minWidth.$ === "MinWidthInBs") {
            var a = minWidth.a;
            return !a;
        } else {
            var a = minWidth.b;
            return !a;
        }
    };
    var $arowM$elm_mixin$Mixin$Html$text = $elm$html$Html$text;
    var $author$project$Neat$renderTexts = F3(function(renderer, _v0, o) {
        var self = _v0.self;
        var consTexts = function(_v1) {
            var t = _v1.a;
            var ts = _v1.b;
            return A2($elm$core$List$cons, t, ts);
        };
        var base = $arowM$elm_mixin$Mixin$batch(_List_fromArray([
            o.mixin,
            self,
            $author$project$Neat$class("textView"),
            A2($arowM$elm_mixin$Mixin$style, "--content-gap-x", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.contentGap.horizontal, renderer.baseSize))),
            A2($arowM$elm_mixin$Mixin$style, "--content-gap-y", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.contentGap.vertical, renderer.baseSize)))
        ]));
        return A3($arowM$elm_mixin$Mixin$Html$node, o.nodeName, _List_fromArray([
            base
        ]), _List_fromArray([
            A2($arowM$elm_mixin$Mixin$Html$div, _List_fromArray([
                $author$project$Neat$class("textView_textMargin")
            ]), A2($elm$core$List$map, function(inline) {
                return A3($arowM$elm_mixin$Mixin$Html$node, inline.nodeName, _List_fromArray([
                    inline.mixin,
                    $author$project$Neat$class("textView_inline")
                ]), _List_fromArray([
                    $arowM$elm_mixin$Mixin$Html$text(inline.text)
                ]));
            }, consTexts(o.texts)))
        ]));
    });
    var $author$project$Neat$renderBoundary = F4(function(renderer, _v10, props, o) {
        var self = _v10.self;
        var overlays = $elm$core$List$reverse(o.overlays);
        var childMixin = {
            inherit: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                function() {
                    var _v13 = o.height;
                    if (_v13.$ === "MinSize") return $author$project$Neat$class("heightMinSize");
                    else return $author$project$Neat$class("heightFlex");
                }(),
                function() {
                    var _v14 = o.width;
                    if (_v14.$ === "MinSize") return $author$project$Neat$class("widthMinSize");
                    else return $author$project$Neat$class("widthFlex");
                }()
            ])),
            self: $author$project$Neat$class("boundaryContent")
        };
        var base = $arowM$elm_mixin$Mixin$batch(_List_fromArray([
            A3($author$project$Neat$boundaryCustomProperty, renderer, props, o),
            childMixin.inherit,
            self,
            $author$project$Neat$class("boundary"),
            o.horizontalOverflow ? $author$project$Neat$class("boundary-horizontalOverflow") : $arowM$elm_mixin$Mixin$none,
            o.verticalOverflow ? $author$project$Neat$class("boundary-verticalOverflow") : $arowM$elm_mixin$Mixin$none,
            $author$project$Neat$hasMaxHeight(o.maxHeight) ? $author$project$Neat$class("boundary-hasMaxHeight") : $arowM$elm_mixin$Mixin$none,
            $author$project$Neat$hasMaxWidth(o.maxWidth) ? $author$project$Neat$class("boundary-hasMaxWidth") : $arowM$elm_mixin$Mixin$none,
            !$author$project$Neat$minHeightZero(o.minHeight) ? $author$project$Neat$class("boundary-hasMinHeight") : $arowM$elm_mixin$Mixin$none,
            !$author$project$Neat$minWidthZero(o.minWidth) ? $author$project$Neat$class("boundary-hasMinWidth") : $arowM$elm_mixin$Mixin$none,
            !$elm$core$List$isEmpty(o.overlays) ? $author$project$Neat$class("boundary-hasOverlays") : $arowM$elm_mixin$Mixin$none,
            o.enforcePointerEvent ? $author$project$Neat$class("boundary-enforcePointerEvent") : $arowM$elm_mixin$Mixin$none
        ]));
        var _v11 = o.content;
        switch(_v11.$){
            case "NoContent":
                return $arowM$elm_mixin$Mixin$Html$text("");
            case "ViewContent":
                var content = _v11.a;
                return o.verticalOverflow && !!o.padding.vertical ? A3($arowM$elm_mixin$Mixin$Html$node, o.nodeName, _List_fromArray([
                    base,
                    $author$project$Neat$class("boundary-verticalOverflow-wrapper")
                ]), _List_fromArray([
                    A3($arowM$elm_mixin$Mixin$Html$keyed, "div", _List_fromArray([
                        o.mixin,
                        $author$project$Neat$class("boundary_scroller"),
                        $author$project$Neat$class("boundary_scroller-verticalScroll")
                    ]), A2($elm$core$List$cons, _Utils_Tuple2("content", A3($author$project$Neat$render_, renderer, childMixin, content)), A2($elm$core$List$map, $author$project$Neat$renderOverlay(renderer), overlays)))
                ])) : _Utils_eq(content, $author$project$Neat$Internal$None) ? A3($arowM$elm_mixin$Mixin$Html$keyed, o.nodeName, _List_fromArray([
                    o.mixin,
                    base,
                    $author$project$Neat$class("boundary-view"),
                    $author$project$Neat$class("boundary-view-noContent")
                ]), A2($elm$core$List$map, $author$project$Neat$renderOverlay(renderer), overlays)) : A3($arowM$elm_mixin$Mixin$Html$keyed, o.nodeName, _List_fromArray([
                    o.mixin,
                    base,
                    $author$project$Neat$class("boundary-view"),
                    $author$project$Neat$class("boundary-view-hasContent")
                ]), A2($elm$core$List$cons, _Utils_Tuple2("content", A3($author$project$Neat$render_, renderer, childMixin, content)), A2($elm$core$List$map, $author$project$Neat$renderOverlay(renderer), overlays)));
            case "HtmlContent":
                var children = _v11.a;
                return A3($arowM$elm_mixin$Mixin$Html$keyed, o.nodeName, _List_fromArray([
                    o.mixin,
                    base,
                    $author$project$Neat$class("boundary-html")
                ]), function(cs) {
                    return _Utils_ap(cs, A2($elm$core$List$map, $author$project$Neat$renderOverlay(renderer), overlays));
                }(A2($elm$core$List$map, function(_v12) {
                    var k = _v12.a;
                    var b = _v12.b;
                    return _Utils_Tuple2(k, A4($author$project$Neat$renderBoundary, renderer, childMixin, {
                        outerGap: $author$project$Neat$emptyGap
                    }, b));
                }, children)));
            case "TextsContent":
                var texts = _v11.a;
                return A3($arowM$elm_mixin$Mixin$Html$node, o.nodeName, _List_fromArray([
                    o.mixin,
                    base,
                    $author$project$Neat$class("boundary-text")
                ]), _List_fromArray([
                    A3($arowM$elm_mixin$Mixin$Html$keyed, "div", _List_fromArray([
                        $author$project$Neat$class("boundary_textMargin")
                    ]), function(children) {
                        return _Utils_ap(children, A2($elm$core$List$map, $author$project$Neat$renderOverlay(renderer), overlays));
                    }(A2($elm$core$List$indexedMap, F2(function(n, inline) {
                        return _Utils_Tuple2("content" + $elm$core$String$fromInt(n), A3($arowM$elm_mixin$Mixin$Html$node, inline.nodeName, _List_fromArray([
                            inline.mixin,
                            $author$project$Neat$class("boundary_text")
                        ]), _List_fromArray([
                            $arowM$elm_mixin$Mixin$Html$text(inline.text)
                        ])));
                    }), texts)))
                ]));
            default:
                var str = _v11.a;
                return A3($arowM$elm_mixin$Mixin$Html$node, o.nodeName, _List_fromArray([
                    o.mixin,
                    base
                ]), _List_fromArray([
                    $arowM$elm_mixin$Mixin$Html$text(str)
                ]));
        }
    });
    var $author$project$Neat$renderColumn = F3(function(renderer, _v6, o) {
        var inherit = _v6.inherit;
        var self = _v6.self;
        var childMixin = function(item) {
            return {
                inherit: inherit,
                self: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                    $author$project$Neat$class("columnChild"),
                    item.grow ? $author$project$Neat$class("columnChild-grow") : $arowM$elm_mixin$Mixin$none,
                    function() {
                        var _v9 = item.alignSelf;
                        switch(_v9.$){
                            case "AlignStart":
                                return $author$project$Neat$class("columnChild-alignStart");
                            case "AlignCenter":
                                return $author$project$Neat$class("columnChild-alignCenter");
                            case "AlignEnd":
                                return $author$project$Neat$class("columnChild-alignEnd");
                            default:
                                return $author$project$Neat$class("columnChild-alignStretch");
                        }
                    }()
                ]))
            };
        };
        var base = $arowM$elm_mixin$Mixin$batch(_List_fromArray([
            o.mixin,
            self,
            $author$project$Neat$class("column"),
            function() {
                var _v8 = o.justifyContent;
                switch(_v8.$){
                    case "AlignStart":
                        return $author$project$Neat$class("column-justifyStart");
                    case "AlignCenter":
                        return $author$project$Neat$class("column-justifyCenter");
                    case "AlignEnd":
                        return $author$project$Neat$class("column-justifyEnd");
                    default:
                        return $author$project$Neat$class("column-justifyStretch");
                }
            }(),
            A2($arowM$elm_mixin$Mixin$style, "--content-gap-x", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.contentGap.horizontal, renderer.baseSize))),
            A2($arowM$elm_mixin$Mixin$style, "--content-gap-y", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.contentGap.vertical, renderer.baseSize)))
        ]));
        var _v7 = o.children;
        if (!_v7.b.b) {
            var item = _v7.a;
            return A3($arowM$elm_mixin$Mixin$Html$keyed, o.nodeName, _List_fromArray([
                base,
                $author$project$Neat$class("column-single")
            ]), _List_fromArray([
                _Utils_Tuple2(item.key, A3($author$project$Neat$render_, renderer, childMixin(item), item.content))
            ]));
        } else {
            var item0 = _v7.a;
            var items = _v7.b;
            return A3($arowM$elm_mixin$Mixin$Html$keyed, o.nodeName, _List_fromArray([
                base,
                $author$project$Neat$class("column-multi")
            ]), A2($elm$core$List$map, function(item) {
                return _Utils_Tuple2(item.key, A3($author$project$Neat$render_, renderer, childMixin(item), item.content));
            }, A2($elm$core$List$cons, item0, items)));
        }
    });
    var $author$project$Neat$renderOverlay = F2(function(renderer, overlay) {
        var childMixin = {
            inherit: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                $author$project$Neat$class("heightFlex"),
                $author$project$Neat$class("widthFlex")
            ])),
            self: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                $author$project$Neat$class("overlay"),
                A2($arowM$elm_mixin$Mixin$style, "--overlay-top", $elm$core$String$fromFloat(overlay.area.top) + "%"),
                A2($arowM$elm_mixin$Mixin$style, "--overlay-bottom", $elm$core$String$fromFloat(overlay.area.bottom) + "%"),
                A2($arowM$elm_mixin$Mixin$style, "--overlay-left", $elm$core$String$fromFloat(overlay.area.left) + "%"),
                A2($arowM$elm_mixin$Mixin$style, "--overlay-right", $elm$core$String$fromFloat(overlay.area.right) + "%"),
                A2($arowM$elm_mixin$Mixin$style, "--overlay-priority", A2($elm$core$Maybe$withDefault, "auto", A2($elm$core$Maybe$map, $elm$core$String$fromInt, overlay.area.priority)))
            ]))
        };
        var _v5 = overlay.boundary;
        var boundary = _v5.a;
        return _Utils_Tuple2("overlay-" + overlay.name, A4($author$project$Neat$renderBoundary, renderer, childMixin, {
            outerGap: $author$project$Neat$emptyGap
        }, _Utils_update(boundary, {
            height: $author$project$Neat$Internal$FlexSize,
            width: $author$project$Neat$Internal$FlexSize
        })));
    });
    var $author$project$Neat$renderRow = F3(function(renderer, _v1, o) {
        var inherit = _v1.inherit;
        var self = _v1.self;
        var childMixin = function(item) {
            return {
                inherit: inherit,
                self: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                    $author$project$Neat$class("rowChild"),
                    item.grow ? $author$project$Neat$class("rowChild-grow") : $arowM$elm_mixin$Mixin$none,
                    function() {
                        var _v4 = item.alignSelf;
                        switch(_v4.$){
                            case "AlignStart":
                                return $author$project$Neat$class("rowChild-alignStart");
                            case "AlignCenter":
                                return $author$project$Neat$class("rowChild-alignCenter");
                            case "AlignEnd":
                                return $author$project$Neat$class("rowChild-alignEnd");
                            default:
                                return $author$project$Neat$class("rowChild-alignStretch");
                        }
                    }()
                ]))
            };
        };
        var base = $arowM$elm_mixin$Mixin$batch(_List_fromArray([
            o.mixin,
            self,
            $author$project$Neat$class("row"),
            o.wrap ? $author$project$Neat$class("row-wrap") : $arowM$elm_mixin$Mixin$none,
            function() {
                var _v3 = o.justifyContent;
                switch(_v3.$){
                    case "AlignStart":
                        return $author$project$Neat$class("row-justifyStart");
                    case "AlignCenter":
                        return $author$project$Neat$class("row-justifyCenter");
                    case "AlignEnd":
                        return $author$project$Neat$class("row-justifyEnd");
                    default:
                        return $author$project$Neat$class("row-justifyStretch");
                }
            }(),
            A2($arowM$elm_mixin$Mixin$style, "--content-gap-x", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.contentGap.horizontal, renderer.baseSize))),
            A2($arowM$elm_mixin$Mixin$style, "--content-gap-y", $author$project$Neat$renderBaseSize(A2($author$project$Neat$multipleBaseSize, o.contentGap.vertical, renderer.baseSize)))
        ]));
        var _v2 = o.children;
        if (!_v2.b.b) {
            var item = _v2.a;
            return A3($arowM$elm_mixin$Mixin$Html$keyed, o.nodeName, _List_fromArray([
                base,
                $author$project$Neat$class("row-single")
            ]), _List_fromArray([
                _Utils_Tuple2(item.key, A3($author$project$Neat$render_, renderer, childMixin(item), item.content))
            ]));
        } else {
            var item0 = _v2.a;
            var items = _v2.b;
            return A3($arowM$elm_mixin$Mixin$Html$keyed, o.nodeName, _List_fromArray([
                base,
                $author$project$Neat$class("row-multi")
            ]), A2($elm$core$List$map, function(item) {
                return _Utils_Tuple2(item.key, A3($author$project$Neat$render_, renderer, childMixin(item), item.content));
            }, A2($elm$core$List$cons, item0, items)));
        }
    });
    var $author$project$Neat$render_ = F3(function(renderer, childMixin, view) {
        switch(view.$){
            case "FromBoundary":
                var outerGap = view.a;
                var o = view.b;
                return A4($author$project$Neat$renderBoundary, renderer, childMixin, {
                    outerGap: outerGap
                }, o);
            case "FromRow":
                var o = view.a;
                return A3($author$project$Neat$renderRow, renderer, childMixin, o);
            case "FromColumn":
                var o = view.a;
                return A3($author$project$Neat$renderColumn, renderer, childMixin, o);
            case "FromTexts":
                var o = view.a;
                return A3($author$project$Neat$renderTexts, renderer, childMixin, o);
            default:
                return $arowM$elm_mixin$Mixin$Html$text("");
        }
    });
    var $author$project$Neat$render = F2(function(_v0, _v1) {
        var renderer = _v0.a;
        var boundary = _v1.a;
        var childMixin = {
            inherit: $arowM$elm_mixin$Mixin$batch(_List_fromArray([
                $author$project$Neat$class("heightFlex"),
                $author$project$Neat$class("widthFlex")
            ])),
            self: $author$project$Neat$class("top")
        };
        return A2($arowM$elm_mixin$Mixin$Html$div, _List_Nil, _List_fromArray([
            A3($arowM$elm_mixin$Mixin$Html$node, "style", _List_fromArray([
                A2($arowM$elm_mixin$Mixin$style, "display", "none")
            ]), _List_fromArray([
                $arowM$elm_mixin$Mixin$Html$text($author$project$Neat$neatLayoutStyle)
            ])),
            A4($author$project$Neat$renderBoundary, renderer, childMixin, {
                outerGap: $author$project$Neat$emptyGap
            }, _Utils_update(boundary, {
                height: $author$project$Neat$Internal$FlexSize,
                width: $author$project$Neat$Internal$FlexSize
            }))
        ]));
    });
    var $author$project$Main$view = function(model) {
        return {
            body: _List_fromArray([
                A2($author$project$Neat$render, $author$project$Neat$defaultRenderer, $author$project$Main$body(model))
            ]),
            title: "Sample"
        };
    };
    var $author$project$Main$main = $elm$browser$Browser$document({
        init: function(_v0) {
            return $author$project$Main$init;
        },
        subscriptions: $author$project$Main$subscriptions,
        update: $author$project$Main$update,
        view: $author$project$Main$view
    });
    _Platform_export({
        "Main": {
            "init": $author$project$Main$main($elm$json$Json$Decode$succeed(_Utils_Tuple0))({
                "versions": {
                    "elm": "0.19.1"
                },
                "types": {
                    "message": "()",
                    "aliases": {},
                    "unions": {}
                }
            })
        }
    });
    //////////////////// HMR BEGIN ////////////////////
    /*
  MIT License http://www.opensource.org/licenses/mit-license.php
  Original Author: Flux Xu @fluxxu
*/ /*
    A note about the environment that this code runs in...

    assumed globals:
        - `module` (from Node.js module system and webpack)

    assumed in scope after injection into the Elm IIFE:
        - `scope` (has an 'Elm' property which contains the public Elm API)
        - various functions defined by Elm which we have to hook such as `_Platform_initialize` and `_Scheduler_binding`
 */ if (module.hot) {
        (function() {
            "use strict";
            //polyfill for IE: https://github.com/fluxxu/elm-hot-loader/issues/16
            if (typeof Object.assign != "function") Object.assign = function(target) {
                "use strict";
                if (target == null) throw new TypeError("Cannot convert undefined or null to object");
                target = Object(target);
                for(var index = 1; index < arguments.length; index++){
                    var source = arguments[index];
                    if (source != null) {
                        for(var key in source)if (Object.prototype.hasOwnProperty.call(source, key)) target[key] = source[key];
                    }
                }
                return target;
            };
            // Elm 0.19.1 introduced a '$' prefix at the beginning of the symbols it emits,
            // and we check for `Maybe.Just` because we expect it to be present in all Elm programs.
            var elmVersion;
            if (typeof elm$core$Maybe$Just !== "undefined") elmVersion = "0.19.0";
            else if (typeof $elm$core$Maybe$Just !== "undefined") elmVersion = "0.19.1";
            else throw new Error("Could not determine Elm version");
            function elmSymbol(symbol) {
                try {
                    switch(elmVersion){
                        case "0.19.0":
                            return eval(symbol);
                        case "0.19.1":
                            return eval("$" + symbol);
                        default:
                            throw new Error("Cannot resolve " + symbol + ". Elm version unknown!");
                    }
                } catch (e) {
                    if (e instanceof ReferenceError) return undefined;
                    else throw e;
                }
            }
            var instances = module.hot.data ? module.hot.data.instances || {} : {};
            var uid = module.hot.data ? module.hot.data.uid || 0 : 0;
            if (Object.keys(instances).length === 0) log("[elm-hot] Enabled");
            var cancellers = [];
            // These 2 variables act as dynamically-scoped variables which are set only when the
            // Elm module's hooked init function is called.
            var initializingInstance = null;
            var swappingInstance = null;
            module.hot.accept();
            module.hot.dispose(function(data) {
                data.instances = instances;
                data.uid = uid;
                // Cleanup pending async tasks
                // First, make sure that no new tasks can be started until we finish replacing the code
                _Scheduler_binding = function() {
                    return _Scheduler_fail(new Error("[elm-hot] Inactive Elm instance."));
                };
                // Second, kill pending tasks belonging to the old instance
                if (cancellers.length) {
                    log("[elm-hot] Killing " + cancellers.length + " running processes...");
                    try {
                        cancellers.forEach(function(cancel) {
                            cancel();
                        });
                    } catch (e) {
                        console.warn("[elm-hot] Kill process error: " + e.message);
                    }
                }
            });
            function log(message) {
                if (module.hot.verbose) console.log(message);
            }
            function getId() {
                return ++uid;
            }
            function findPublicModules(parent, path) {
                var modules = [];
                for(var key in parent){
                    var child = parent[key];
                    var currentPath = path ? path + "." + key : key;
                    if ("init" in child) modules.push({
                        path: currentPath,
                        module: child
                    });
                    else modules = modules.concat(findPublicModules(child, currentPath));
                }
                return modules;
            }
            function registerInstance(domNode, flags, path, portSubscribes, portSends) {
                var id = getId();
                var instance = {
                    id: id,
                    path: path,
                    domNode: domNode,
                    flags: flags,
                    portSubscribes: portSubscribes,
                    portSends: portSends,
                    lastState: null // last Elm app state (root model)
                };
                return instances[id] = instance;
            }
            function isFullscreenApp() {
                // Returns true if the Elm app will take over the entire DOM body.
                return typeof elmSymbol("elm$browser$Browser$application") !== "undefined" || typeof elmSymbol("elm$browser$Browser$document") !== "undefined";
            }
            function wrapDomNode(node) {
                // When embedding an Elm app into a specific DOM node, Elm will replace the provided
                // DOM node with the Elm app's content. When the Elm app is compiled normally, the
                // original DOM node is reused (its attributes and content changes, but the object
                // in memory remains the same). But when compiled using `--debug`, Elm will completely
                // destroy the original DOM node and instead replace it with 2 brand new nodes: one
                // for your Elm app's content and the other for the Elm debugger UI. In this case,
                // if you held a reference to the DOM node provided for embedding, it would be orphaned
                // after Elm module initialization.
                //
                // So in order to make both cases consistent and isolate us from changes in how Elm
                // does this, we will insert a dummy node to wrap the node for embedding and hold
                // a reference to the dummy node.
                //
                // We will also put a tag on the dummy node so that the Elm developer knows who went
                // behind their back and rudely put stuff in their DOM.
                var dummyNode = document.createElement("div");
                dummyNode.setAttribute("data-elm-hot", "true");
                dummyNode.style.height = "inherit";
                var parentNode = node.parentNode;
                parentNode.replaceChild(dummyNode, node);
                dummyNode.appendChild(node);
                return dummyNode;
            }
            function wrapPublicModule(path, module1) {
                var originalInit = module1.init;
                if (originalInit) module1.init = function(args) {
                    var elm;
                    var portSubscribes = {};
                    var portSends = {};
                    var domNode = null;
                    var flags = null;
                    if (typeof args !== "undefined") {
                        // normal case
                        domNode = args["node"] && !isFullscreenApp() ? wrapDomNode(args["node"]) : document.body;
                        flags = args["flags"];
                    } else {
                        // rare case: Elm allows init to be called without any arguments at all
                        domNode = document.body;
                        flags = undefined;
                    }
                    initializingInstance = registerInstance(domNode, flags, path, portSubscribes, portSends);
                    elm = originalInit(args);
                    wrapPorts(elm, portSubscribes, portSends);
                    initializingInstance = null;
                    return elm;
                };
                else console.error("Could not find a public module to wrap at path " + path);
            }
            function swap(Elm, instance) {
                log("[elm-hot] Hot-swapping module: " + instance.path);
                swappingInstance = instance;
                // remove from the DOM everything that had been created by the old Elm app
                var containerNode = instance.domNode;
                while(containerNode.lastChild)containerNode.removeChild(containerNode.lastChild);
                var m = getAt(instance.path.split("."), Elm);
                var elm;
                if (m) {
                    // prepare to initialize the new Elm module
                    var args = {
                        flags: instance.flags
                    };
                    if (containerNode === document.body) ;
                    else {
                        // embed case: provide a new node for Elm to use
                        var nodeForEmbed = document.createElement("div");
                        containerNode.appendChild(nodeForEmbed);
                        args["node"] = nodeForEmbed;
                    }
                    elm = m.init(args);
                    Object.keys(instance.portSubscribes).forEach(function(portName) {
                        if (portName in elm.ports && "subscribe" in elm.ports[portName]) {
                            var handlers = instance.portSubscribes[portName];
                            if (!handlers.length) return;
                            log("[elm-hot] Reconnect " + handlers.length + " handler(s) to port '" + portName + "' (" + instance.path + ").");
                            handlers.forEach(function(handler) {
                                elm.ports[portName].subscribe(handler);
                            });
                        } else {
                            delete instance.portSubscribes[portName];
                            log("[elm-hot] Port was removed: " + portName);
                        }
                    });
                    Object.keys(instance.portSends).forEach(function(portName) {
                        if (portName in elm.ports && "send" in elm.ports[portName]) {
                            log("[elm-hot] Replace old port send with the new send");
                            instance.portSends[portName] = elm.ports[portName].send;
                        } else {
                            delete instance.portSends[portName];
                            log("[elm-hot] Port was removed: " + portName);
                        }
                    });
                } else log("[elm-hot] Module was removed: " + instance.path);
                swappingInstance = null;
            }
            function wrapPorts(elm, portSubscribes, portSends) {
                var portNames = Object.keys(elm.ports || {});
                //hook ports
                if (portNames.length) {
                    // hook outgoing ports
                    portNames.filter(function(name) {
                        return "subscribe" in elm.ports[name];
                    }).forEach(function(portName) {
                        var port = elm.ports[portName];
                        var subscribe = port.subscribe;
                        var unsubscribe = port.unsubscribe;
                        elm.ports[portName] = Object.assign(port, {
                            subscribe: function(handler) {
                                log("[elm-hot] ports." + portName + ".subscribe called.");
                                if (!portSubscribes[portName]) portSubscribes[portName] = [
                                    handler
                                ];
                                else //TODO handle subscribing to single handler more than once?
                                portSubscribes[portName].push(handler);
                                return subscribe.call(port, handler);
                            },
                            unsubscribe: function(handler) {
                                log("[elm-hot] ports." + portName + ".unsubscribe called.");
                                var list = portSubscribes[portName];
                                if (list && list.indexOf(handler) !== -1) list.splice(list.lastIndexOf(handler), 1);
                                else console.warn("[elm-hot] ports." + portName + ".unsubscribe: handler not subscribed");
                                return unsubscribe.call(port, handler);
                            }
                        });
                    });
                    // hook incoming ports
                    portNames.filter(function(name) {
                        return "send" in elm.ports[name];
                    }).forEach(function(portName) {
                        var port = elm.ports[portName];
                        portSends[portName] = port.send;
                        elm.ports[portName] = Object.assign(port, {
                            send: function(val) {
                                return portSends[portName].call(port, val);
                            }
                        });
                    });
                }
                return portSubscribes;
            }
            /*
        Breadth-first search for a `Browser.Navigation.Key` in the user's app model.
        Returns the key and keypath or null if not found.
        */ function findNavKey(rootModel) {
                var queue = [];
                if (isDebuggerModel(rootModel)) /*
                 Extract the user's app model from the Elm Debugger's model. The Elm debugger
                 can hold multiple references to the user's model (e.g. in its "history"). So
                 we must be careful to only search within the "state" part of the Debugger.
                */ queue.push({
                    value: rootModel["state"],
                    keypath: [
                        "state"
                    ]
                });
                else queue.push({
                    value: rootModel,
                    keypath: []
                });
                while(queue.length !== 0){
                    var item = queue.shift();
                    if (typeof item.value === "undefined" || item.value === null) continue;
                    // The nav key is identified by a runtime tag added by the elm-hot injector.
                    if (item.value.hasOwnProperty("elm-hot-nav-key")) // found it!
                    return item;
                    if (typeof item.value !== "object") continue;
                    for(var propName in item.value){
                        if (!item.value.hasOwnProperty(propName)) continue;
                        var newKeypath = item.keypath.slice();
                        newKeypath.push(propName);
                        queue.push({
                            value: item.value[propName],
                            keypath: newKeypath
                        });
                    }
                }
                return null;
            }
            function isDebuggerModel(model) {
                // Up until elm/browser 1.0.2, the Elm debugger could be identified by a
                // property named "expando". But in version 1.0.2 that was renamed to "expandoModel"
                return model && (model.hasOwnProperty("expando") || model.hasOwnProperty("expandoModel")) && model.hasOwnProperty("state");
            }
            function getAt(keyPath, obj) {
                return keyPath.reduce(function(xs, x) {
                    return xs && xs[x] ? xs[x] : null;
                }, obj);
            }
            function removeNavKeyListeners(navKey) {
                window.removeEventListener("popstate", navKey.value);
                window.navigator.userAgent.indexOf("Trident") < 0 || window.removeEventListener("hashchange", navKey.value);
            }
            // hook program creation
            var initialize = _Platform_initialize;
            _Platform_initialize = function(flagDecoder, args, init, update, subscriptions, stepperBuilder) {
                var instance = initializingInstance || swappingInstance;
                var tryFirstRender = !!swappingInstance;
                var hookedInit = function(args) {
                    var initialStateTuple = init(args);
                    if (swappingInstance) {
                        var oldModel = swappingInstance.lastState;
                        var newModel = initialStateTuple.a;
                        if (typeof elmSymbol("elm$browser$Browser$application") !== "undefined") {
                            var oldKeyLoc = findNavKey(oldModel);
                            // attempt to find the Browser.Navigation.Key in the newly-constructed model
                            // and bring it along with the rest of the old data.
                            var newKeyLoc = findNavKey(newModel);
                            var error = null;
                            if (newKeyLoc === null) error = "could not find Browser.Navigation.Key in the new app model";
                            else if (oldKeyLoc === null) error = "could not find Browser.Navigation.Key in the old app model.";
                            else if (newKeyLoc.keypath.toString() !== oldKeyLoc.keypath.toString()) error = "the location of the Browser.Navigation.Key in the model has changed.";
                            else {
                                // remove event listeners attached to the old nav key
                                removeNavKeyListeners(oldKeyLoc.value);
                                // insert the new nav key into the old model in the exact same location
                                var parentKeyPath = oldKeyLoc.keypath.slice(0, -1);
                                var lastSegment = oldKeyLoc.keypath.slice(-1)[0];
                                var oldParent = getAt(parentKeyPath, oldModel);
                                oldParent[lastSegment] = newKeyLoc.value;
                            }
                            if (error !== null) {
                                console.error("[elm-hot] Hot-swapping " + instance.path + " not possible: " + error);
                                oldModel = newModel;
                            }
                        }
                        // the heart of the app state hot-swap
                        initialStateTuple.a = oldModel;
                        // ignore any Cmds returned by the init during hot-swap
                        initialStateTuple.b = elmSymbol("elm$core$Platform$Cmd$none");
                    } else // capture the initial state for later
                    initializingInstance.lastState = initialStateTuple.a;
                    return initialStateTuple;
                };
                var hookedStepperBuilder = function(sendToApp, model) {
                    var result;
                    // first render may fail if shape of model changed too much
                    if (tryFirstRender) {
                        tryFirstRender = false;
                        try {
                            result = stepperBuilder(sendToApp, model);
                        } catch (e) {
                            throw new Error("[elm-hot] Hot-swapping " + instance.path + " is not possible, please reload page. Error: " + e.message);
                        }
                    } else result = stepperBuilder(sendToApp, model);
                    return function(nextModel, isSync) {
                        if (instance) // capture the state after every step so that later we can restore from it during a hot-swap
                        instance.lastState = nextModel;
                        return result(nextModel, isSync);
                    };
                };
                return initialize(flagDecoder, args, hookedInit, update, subscriptions, hookedStepperBuilder);
            };
            // hook process creation
            var originalBinding = _Scheduler_binding;
            _Scheduler_binding = function(originalCallback) {
                return originalBinding(function() {
                    // start the scheduled process, which may return a cancellation function.
                    var cancel = originalCallback.apply(this, arguments);
                    if (cancel) {
                        cancellers.push(cancel);
                        return function() {
                            cancellers.splice(cancellers.indexOf(cancel), 1);
                            return cancel();
                        };
                    }
                    return cancel;
                });
            };
            scope["_elm_hot_loader_init"] = function(Elm) {
                // swap instances
                var removedInstances = [];
                for(var id in instances){
                    var instance = instances[id];
                    if (instance.domNode.parentNode) swap(Elm, instance);
                    else removedInstances.push(id);
                }
                removedInstances.forEach(function(id) {
                    delete instance[id];
                });
                // wrap all public modules
                var publicModules = findPublicModules(Elm);
                publicModules.forEach(function(m) {
                    wrapPublicModule(m.path, m.module);
                });
            };
        })();
        scope["_elm_hot_loader_init"](scope["Elm"]);
    }
//////////////////// HMR END ////////////////////
})(this);

},{}]},["InXGp","bMtV0"], "bMtV0", "parcelRequire94c2")

//# sourceMappingURL=index.4ed6b15e.js.map
