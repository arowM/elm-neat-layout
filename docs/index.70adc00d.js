var t={};!function(t){function e(t,e,n){return n.a=t,n.f=e,n}function n(t){return e(2,t,(function(e){return function(n){return t(e,n)}}))}function a(t){return e(3,t,(function(e){return function(n){return function(a){return t(e,n,a)}}}))}function r(t){return e(4,t,(function(e){return function(n){return function(a){return function(r){return t(e,n,a,r)}}}}))}function o(t,e,n){return 2===t.a?t.f(e,n):t(e)(n)}function u(t,e,n,a){return 3===t.a?t.f(e,n,a):t(e)(n)(a)}function i(t,e,n,a,r){return 4===t.a?t.f(e,n,a,r):t(e)(n)(a)(r)}function l(t,e){for(var n,a=[],r=f(t,e,0,a);r&&(n=a.pop());r=f(n.a,n.b,0,a));return r}function f(t,e,n,a){if(n>100)return a.push(d(t,e)),!0;if(t===e)return!0;if("object"!=typeof t||null===t||null===e)return"function"==typeof t&&w(5),!1;for(var r in 0>t.$&&(t=Bt(t),e=Bt(e)),t)if(!f(t[r],e[r],n+1,a))return!1;return!0}function c(t,e,n){if("object"!=typeof t)return t===e?0:e>t?-1:1;if(void 0===t.$)return(n=c(t.a,e.a))||(n=c(t.b,e.b))?n:c(t.c,e.c);for(;t.b&&e.b&&!(n=c(t.a,e.a));t=t.b,e=e.b);return n||(t.b?1:e.b?-1:0)}function d(t,e){return{a:t,b:e}}function y(t,e){var n={};for(var a in t)n[a]=t[a];for(var a in e)n[a]=e[a];return n}function h(t,e){if("string"==typeof t)return t+e;if(!t.b)return e;var n=v(t.a,e);t=t.b;for(var a=n;t.b;t=t.b)a=a.b=v(t.a,e);return n}var m={$:0};function v(t,e){return{$:1,a:t,b:e}}var s=n(v);function b(t){for(var e=m,n=t.length;n--;)e=v(t[n],e);return e}var N=a((function(t,e,n){for(var a=[];e.b&&n.b;e=e.b,n=n.b)a.push(o(t,e.a,n.a));return b(a)})),L=a((function(t,e,n){for(var a=Array(t),r=0;t>r;r++)a[r]=n(e+r);return a})),g=n((function(t,e){for(var n=Array(t),a=0;t>a&&e.b;a++)n[a]=e.a,e=e.b;return n.length=a,d(n,e)}));function w(t){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+t+".md")}var x=Math.ceil,p=Math.floor,C=Math.log,$=n((function(t,e){return e.join(t)}));function j(t){return t+""}function _(t){return{$:2,b:t}}function M(t,e){return{$:9,f:t,g:e}}_((function(t){return"number"!=typeof t?W("an INT",t):t>-2147483647&&2147483647>t&&(0|t)===t?Qt(t):!isFinite(t)||t%1?W("an INT",t):Qt(t)})),_((function(t){return"boolean"==typeof t?Qt(t):W("a BOOL",t)})),_((function(t){return"number"==typeof t?Qt(t):W("a FLOAT",t)})),_((function(t){return Qt(t)})),_((function(t){return"string"==typeof t?Qt(t):t instanceof String?Qt(t+""):W("a STRING",t)}));var O=n((function(t,e){return M(t,[e])})),S=a((function(t,e,n){return M(t,[e,n])})),F=n((function(t,e){return k(t,e)}));function k(t,e){switch(t.$){case 2:return t.b(e);case 5:return null===e?Qt(t.c):W("null",e);case 3:return z(e)?A(t.b,e,b):W("a LIST",e);case 4:return z(e)?A(t.b,e,E):W("an ARRAY",e);case 6:var n=t.d;if("object"!=typeof e||null===e||!(n in e))return W("an OBJECT with a field named `"+n+"`",e);var a=k(t.b,e[n]);return xe(a)?a:Vt(o(Pt,n,a.a));case 7:var r=t.e;return z(e)?e.length>r?(a=k(t.b,e[r]),xe(a)?a:Vt(o(Yt,r,a.a))):W("a LONGER array. Need index "+r+" but only see "+e.length+" entries",e):W("an ARRAY",e);case 8:if("object"!=typeof e||null===e||z(e))return W("an OBJECT",e);var u=m;for(var i in e)if(e.hasOwnProperty(i)){if(a=k(t.b,e[i]),!xe(a))return Vt(o(Pt,i,a.a));u=v(d(i,a.a),u)}return Qt(re(u));case 9:for(var l=t.f,f=t.g,c=0;f.length>c;c++){if(a=k(f[c],e),!xe(a))return a;l=l(a.a)}return Qt(l);case 10:return a=k(t.b,e),xe(a)?k(t.h(a.a),e):a;case 11:for(var y=m,h=t.g;h.b;h=h.b){if(a=k(h.a,e),xe(a))return a;y=v(a.a,y)}return Vt(Zt(re(y)));case 1:return Vt(o(Xt,t.a,e));case 0:return Qt(t.a)}}function A(t,e,n){for(var a=e.length,r=Array(a),u=0;a>u;u++){var i=k(t,e[u]);if(!xe(i))return Vt(o(Yt,u,i.a));r[u]=i.a}return Qt(n(r))}function z(t){return Array.isArray(t)||"undefined"!=typeof FileList&&t instanceof FileList}function E(t){return o(we,t.length,(function(e){return t[e]}))}function W(t,e){return Vt(o(Xt,"Expecting "+t,e))}function H(t,e){if(t===e)return!0;if(t.$!==e.$)return!1;switch(t.$){case 0:case 1:return t.a===e.a;case 2:return t.b===e.b;case 5:return t.c===e.c;case 3:case 4:case 8:return H(t.b,e.b);case 6:return t.d===e.d&&H(t.b,e.b);case 7:return t.e===e.e&&H(t.b,e.b);case 9:return t.f===e.f&&I(t.g,e.g);case 10:return t.h===e.h&&H(t.b,e.b);case 11:return I(t.g,e.g)}}function I(t,e){var n=t.length;if(n!==e.length)return!1;for(var a=0;n>a;a++)if(!H(t[a],e[a]))return!1;return!0}function T(t){return{$:0,a:t}}function J(t){return{$:2,b:t,c:null}}var q=n((function(t,e){return{$:3,b:t,d:e}})),B=0;function V(t){var e={$:0,e:B++,f:t,g:null,h:[]};return Y(e),e}var X=!1,P=[];function Y(t){if(P.push(t),!X){for(X=!0;t=P.shift();)Q(t);X=!1}}function Q(t){for(;t.f;){var e=t.f.$;if(0===e||1===e){for(;t.g&&t.g.$!==e;)t.g=t.g.i;if(!t.g)return;t.f=t.g.b(t.f.a),t.g=t.g.i}else{if(2===e)return void(t.f.c=t.f.b((function(e){t.f=e,Y(t)})));if(5===e){if(0===t.h.length)return;t.f=t.f.b(t.h.shift())}else t.g={$:3===e?0:1,b:t.f.b,i:t.g},t.f=t.f.d}}}var Z={};function R(t,e){var n={g:e,h:void 0},a=t.c,r=t.d,l=t.e,f=t.f;return n.h=V(o(q,(function t(e){return o(q,t,{$:5,b:function(t){var o=t.a;return 0===t.$?u(r,n,o,e):l&&f?i(a,n,o.i,o.j,e):u(a,n,l?o.i:o.j,e)}})}),t.b))}var D,U=n((function(t,e){return J((function(n){t.g(e),n(T(0))}))}));function G(t){return{$:2,m:t}}function K(t,e,n){var a,r={};for(var o in tt(!0,e,r,null),tt(!1,n,r,null),t)(a=t[o]).h.push({$:"fx",a:r[o]||{i:m,j:m}}),Y(a)}function tt(t,e,n,a){switch(e.$){case 1:var r=e.k,u=function(t,e,n,a){return o(t?Z[e].e:Z[e].f,(function(t){for(var e=n;e;e=e.q)t=e.p(t);return t}),a)}(t,r,a,e.l);return void(n[r]=function(t,e,n){return n=n||{i:m,j:m},t?n.i=v(e,n.i):n.j=v(e,n.j),n}(t,u,n[r]));case 2:for(var i=e.m;i.b;i=i.b)tt(t,i.a,n,a);return;case 3:return void tt(t,e.o,n,{p:e.n,q:a})}}var et="undefined"!=typeof document?document:{};function nt(t,e){t.appendChild(e)}function at(t){return{$:0,a:t}}var rt=n((function(t,e){return n((function(n,a){for(var r=[],o=0;a.b;a=a.b){var u=a.a;o+=u.b||0,r.push(u)}return o+=r.length,{$:1,c:e,d:mt(n),e:r,f:t,b:o}}))}))(void 0),ot=n((function(t,e){return n((function(n,a){for(var r=[],o=0;a.b;a=a.b){var u=a.a;o+=u.b.b||0,r.push(u)}return o+=r.length,{$:2,c:e,d:mt(n),e:r,f:t,b:o}}))}))(void 0),ut=n((function(t,e){return{$:"a0",n:t,o:e}})),it=n((function(t,e){return{$:"a2",n:t,o:e}})),lt=n((function(t,e){return{$:"a3",n:t,o:e}}));function ft(t){return"script"==t?"p":t}var ct,dt=n((function(t,e){return"a0"===e.$?o(ut,e.n,function(t,e){var n=je(e);return{$:e.$,a:n?u(Ce,3>n?yt:ht,$e(t),e.a):o(pe,t,e.a)}}(t,e.o)):e})),yt=n((function(t,e){return d(t(e.a),e.b)})),ht=n((function(t,e){return{x:t(e.x),am:e.am,aj:e.aj}}));function mt(t){for(var e={};t.b;t=t.b){var n=t.a,a=n.$,r=n.n,o=n.o;if("a2"!==a){var u=e[a]||(e[a]={});"a3"===a&&"class"===r?vt(u,r,o):u[r]=o}else"className"===r?vt(e,r,o):e[r]=o}return e}function vt(t,e,n){var a=t[e];t[e]=a?a+" "+n:n}function st(t,e){var n=t.$;if(5===n)return st(t.k||(t.k=t.m()),e);if(0===n)return et.createTextNode(t.a);if(4===n){for(var a=t.k,r=t.j;4===a.$;)"object"!=typeof r?r=[r,a.j]:r.push(a.j),a=a.k;var o={j:r,p:e};return(u=st(a,o)).elm_event_node_ref=o,u}if(3===n)return bt(u=t.h(t.g),e,t.d),u;var u=t.f?et.createElementNS(t.f,t.c):et.createElement(t.c);D&&"a"==t.c&&u.addEventListener("click",D(u)),bt(u,e,t.d);for(var i=t.e,l=0;i.length>l;l++)nt(u,st(1===n?i[l]:i[l].b,e));return u}function bt(t,e,n){for(var a in n){var r=n[a];"a1"===a?Nt(t,r):"a0"===a?wt(t,e,r):"a3"===a?Lt(t,r):"a4"===a?gt(t,r):("value"!==a&&"checked"!==a||t[a]!==r)&&(t[a]=r)}}function Nt(t,e){var n=t.style;for(var a in e)n[a]=e[a]}function Lt(t,e){for(var n in e){var a=e[n];void 0!==a?t.setAttribute(n,a):t.removeAttribute(n)}}function gt(t,e){for(var n in e){var a=e[n],r=a.f,o=a.o;void 0!==o?t.setAttributeNS(r,n,o):t.removeAttributeNS(r,n)}}function wt(t,e,n){var a=t.elmFs||(t.elmFs={});for(var r in n){var o=n[r],u=a[r];if(o){if(u){if(u.q.$===o.$){u.q=o;continue}t.removeEventListener(r,u)}u=xt(e,o),t.addEventListener(r,u,ct&&{passive:2>je(o)}),a[r]=u}else t.removeEventListener(r,u),a[r]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){ct=!0}}))}catch(t){}function xt(t,e){function n(e){var a=n.q,r=k(a.a,e);if(xe(r)){for(var o,u=je(a),i=r.a,l=u?3>u?i.a:i.x:i,f=1==u?i.b:3==u&&i.am,c=(f&&e.stopPropagation(),(2==u?i.b:3==u&&i.aj)&&e.preventDefault(),t);o=c.j;){if("function"==typeof o)l=o(l);else for(var d=o.length;d--;)l=o[d](l);c=c.p}c(l,f)}}return n.q=e,n}function pt(t,e){return t.$==e.$&&H(t.a,e.a)}function Ct(t,e,n,a){var r={$:e,r:n,s:a,t:void 0,u:void 0};return t.push(r),r}function $t(t,e,n,a){if(t!==e){var r=t.$,o=e.$;if(r!==o){if(1!==r||2!==o)return void Ct(n,0,a,e);e=function(t){for(var e=t.e,n=e.length,a=Array(n),r=0;n>r;r++)a[r]=e[r].b;return{$:1,c:t.c,d:t.d,e:a,f:t.f,b:t.b}}(e),o=1}switch(o){case 5:for(var u=t.l,i=e.l,l=u.length,f=l===i.length;f&&l--;)f=u[l]===i[l];if(f)return void(e.k=t.k);e.k=e.m();var c=[];return $t(t.k,e.k,c,0),void(c.length>0&&Ct(n,1,a,c));case 4:for(var d=t.j,y=e.j,h=!1,m=t.k;4===m.$;)h=!0,"object"!=typeof d?d=[d,m.j]:d.push(m.j),m=m.k;for(var v=e.k;4===v.$;)h=!0,"object"!=typeof y?y=[y,v.j]:y.push(v.j),v=v.k;return h&&d.length!==y.length?void Ct(n,0,a,e):((h?function(t,e){for(var n=0;t.length>n;n++)if(t[n]!==e[n])return!1;return!0}(d,y):d===y)||Ct(n,2,a,y),void $t(m,v,n,a+1));case 0:return void(t.a!==e.a&&Ct(n,3,a,e.a));case 1:return void jt(t,e,n,a,Mt);case 2:return void jt(t,e,n,a,Ot);case 3:if(t.h!==e.h)return void Ct(n,0,a,e);var s=_t(t.d,e.d);s&&Ct(n,4,a,s);var b=e.i(t.g,e.g);return void(b&&Ct(n,5,a,b))}}}function jt(t,e,n,a,r){if(t.c===e.c&&t.f===e.f){var o=_t(t.d,e.d);o&&Ct(n,4,a,o),r(t,e,n,a)}else Ct(n,0,a,e)}function _t(t,e,n){var a;for(var r in t)if("a1"!==r&&"a0"!==r&&"a3"!==r&&"a4"!==r)if(r in e){var o=t[r],u=e[r];o===u&&"value"!==r&&"checked"!==r||"a0"===n&&pt(o,u)||((a=a||{})[r]=u)}else(a=a||{})[r]=n?"a1"===n?"":"a0"===n||"a3"===n?void 0:{f:t[r].f,o:void 0}:"string"==typeof t[r]?"":null;else{var i=_t(t[r],e[r]||{},r);i&&((a=a||{})[r]=i)}for(var l in e)l in t||((a=a||{})[l]=e[l]);return a}function Mt(t,e,n,a){var r=t.e,o=e.e,u=r.length,i=o.length;u>i?Ct(n,6,a,{v:i,i:u-i}):i>u&&Ct(n,7,a,{v:u,e:o});for(var l=i>u?u:i,f=0;l>f;f++){var c=r[f];$t(c,o[f],n,++a),a+=c.b||0}}function Ot(t,e,n,a){for(var r=[],o={},u=[],i=t.e,l=e.e,f=i.length,c=l.length,d=0,y=0,h=a;f>d&&c>y;){var m=(j=i[d]).a,v=(_=l[y]).a,s=j.b,b=_.b,N=void 0,L=void 0;if(m!==v){var g=i[d+1],w=l[y+1];if(g){var x=g.a,p=g.b;L=v===x}if(w){var C=w.a,$=w.b;N=m===C}if(N&&L)$t(s,$,r,++h),Ft(o,r,m,b,y,u),h+=s.b||0,kt(o,r,m,p,++h),h+=p.b||0,d+=2,y+=2;else if(N)h++,Ft(o,r,v,b,y,u),$t(s,$,r,h),h+=s.b||0,d+=1,y+=2;else if(L)kt(o,r,m,s,++h),h+=s.b||0,$t(p,b,r,++h),h+=p.b||0,d+=2,y+=1;else{if(!g||x!==C)break;kt(o,r,m,s,++h),Ft(o,r,v,b,y,u),h+=s.b||0,$t(p,$,r,++h),h+=p.b||0,d+=2,y+=2}}else $t(s,b,r,++h),h+=s.b||0,d++,y++}for(;f>d;){var j;h++,kt(o,r,(j=i[d]).a,s=j.b,h),h+=s.b||0,d++}for(;c>y;){var _,M=M||[];Ft(o,r,(_=l[y]).a,_.b,void 0,M),y++}(r.length>0||u.length>0||M)&&Ct(n,8,a,{w:r,x:u,y:M})}var St="_elmW6BL";function Ft(t,e,n,a,r,o){var u=t[n];if(!u)return o.push({r:r,A:u={c:0,z:a,r:r,s:void 0}}),void(t[n]=u);if(1===u.c){o.push({r:r,A:u}),u.c=2;var i=[];return $t(u.z,a,i,u.r),u.r=r,void(u.s.s={w:i,A:u})}Ft(t,e,n+St,a,r,o)}function kt(t,e,n,a,r){var o=t[n];if(o){if(0===o.c){o.c=2;var u=[];return $t(a,o.z,u,r),void Ct(e,9,r,{w:u,A:o})}kt(t,e,n+St,a,r)}else{var i=Ct(e,9,r,void 0);t[n]={c:1,z:a,r:r,s:i}}}function At(t,e,n,a){zt(t,e,n,0,0,e.b,a)}function zt(t,e,n,a,r,o,u){for(var i=n[a],l=i.r;l===r;){var f=i.$;if(1===f)At(t,e.k,i.s,u);else if(8===f)i.t=t,i.u=u,(c=i.s.w).length>0&&zt(t,e,c,0,r,o,u);else if(9===f){i.t=t,i.u=u;var c,d=i.s;d&&(d.A.s=t,(c=d.w).length>0&&zt(t,e,c,0,r,o,u))}else i.t=t,i.u=u;if(!(i=n[++a])||(l=i.r)>o)return a}var y=e.$;if(4===y){for(var h=e.k;4===h.$;)h=h.k;return zt(t,h,n,a,r+1,o,t.elm_event_node_ref)}for(var m=e.e,v=t.childNodes,s=0;m.length>s;s++){r++;var b=1===y?m[s]:m[s].b,N=r+(b.b||0);if(!(r>l||l>N||(i=n[a=zt(v[s],b,n,a,r,N,u)])&&(l=i.r)<=o))return a;r=N}return a}function Et(t,e){for(var n=0;e.length>n;n++){var a=e[n],r=a.t,o=Wt(r,a);r===t&&(t=o)}return t}function Wt(t,e){switch(e.$){case 0:return function(t,e,n){var a=t.parentNode,r=st(e,n);return r.elm_event_node_ref||(r.elm_event_node_ref=t.elm_event_node_ref),a&&r!==t&&a.replaceChild(r,t),r}(t,e.s,e.u);case 4:return bt(t,e.u,e.s),t;case 3:return t.replaceData(0,t.length,e.s),t;case 1:return Et(t,e.s);case 2:return t.elm_event_node_ref?t.elm_event_node_ref.j=e.s:t.elm_event_node_ref={j:e.s,p:e.u},t;case 6:for(var n=e.s,a=0;n.i>a;a++)t.removeChild(t.childNodes[n.v]);return t;case 7:for(var r=(n=e.s).e,o=t.childNodes[a=n.v];r.length>a;a++)t.insertBefore(st(r[a],e.u),o);return t;case 9:if(!(n=e.s))return t.parentNode.removeChild(t),t;var u=n.A;return void 0!==u.r&&t.parentNode.removeChild(t),u.s=Et(t,n.w),t;case 8:return function(t,e){var n=e.s,a=function(t,e){if(t){for(var n=et.createDocumentFragment(),a=0;t.length>a;a++){var r=t[a].A;nt(n,2===r.c?r.s:st(r.z,e.u))}return n}}(n.y,e);t=Et(t,n.w);for(var r=n.x,o=0;r.length>o;o++){var u=r[o],i=u.A,l=2===i.c?i.s:st(i.z,e.u);t.insertBefore(l,t.childNodes[u.r])}return a&&nt(t,a),t}(t,e);case 5:return e.s(t);default:w(10)}}function Ht(t){if(3===t.nodeType)return at(t.textContent);if(1!==t.nodeType)return at("");for(var e=m,n=t.attributes,a=n.length;a--;){var r=n[a];e=v(o(lt,r.name,r.value),e)}var i=t.tagName.toLowerCase(),l=m,f=t.childNodes;for(a=f.length;a--;)l=v(Ht(f[a]),l);return u(rt,i,e,l)}var It=r((function(t,e,n,a){return function(t,e,n,a,r,u){var i=o(F,t,e?e.flags:void 0);xe(i)||w(2);var l={},f=(i=n(i.a)).a,c=u(y,f),d=function(t,e){var n;for(var a in Z){var r=Z[a];r.a&&((n=n||{})[a]=r.a(a,e)),t[a]=R(r,e)}return n}(l,y);function y(t,e){c(f=(i=o(a,t,f)).a,e),K(l,i.b,r(f))}return K(l,i.b,r(f)),d?{ports:d}:{}}(e,a,t.a0,t.bb,t.a8,(function(e,n){var a=t.ak&&t.ak(e),r=t.bc,o=et.title,u=et.body,i=Ht(u);return function(t,e){e(t);var n=0;function a(){n=1===n?0:(Tt(a),e(t),1)}return function(r,o){t=r,o?(e(t),2===n&&(n=1)):(0===n&&Tt(a),n=2)}}(n,(function(t){D=a;var n=r(t),l=rt("body")(m)(n.aT),f=function(t,e){var n=[];return $t(t,e,n,0),n}(i,l);u=function(t,e,n,a){return 0===n.length?t:(At(t,e,n,a),Et(t,n))}(u,i,f,e),i=l,D=0,o!==n.a9&&(et.title=o=n.a9)}))}))})),Tt=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(t){return setTimeout(t,1e3/60)});"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Jt=s,qt=a((function(t,e,n){for(;;){if(-2===n.$)return e;var a=n.d,r=t,o=u(t,n.b,n.c,u(qt,t,e,n.e));t=r,e=o,n=a}})),Bt=function(t){return u(qt,a((function(t,e,n){return o(Jt,d(t,e),n)})),m,t)},Vt=function(t){return{$:1,a:t}},Xt=n((function(t,e){return{$:3,a:t,b:e}})),Pt=n((function(t,e){return{$:0,a:t,b:e}})),Yt=n((function(t,e){return{$:1,a:t,b:e}})),Qt=function(t){return{$:0,a:t}},Zt=function(t){return{$:2,a:t}},Rt=function(t){return{$:0,a:t}},Dt={$:1},Ut=j,Gt=n((function(t,e){return o($,t,function(t){for(var e=[];t.b;t=t.b)e.push(t.a);return e}(e))})),Kt=a((function(t,e,n){for(;;){if(!n.b)return e;var a=n.b,r=t,u=o(t,n.a,e);t=r,e=u,n=a}})),te=N,ee=a((function(t,e,n){for(;;){if(c(t,e)>=1)return n;var a=t,r=e-1,u=o(Jt,e,n);t=a,e=r,n=u}})),ne=n((function(t,e){return u(ee,t,e,m)})),ae=n((function(t,e){return u(te,t,o(ne,0,function(t){return u(Kt,n((function(t,e){return e+1})),0,t)}(e)-1),e)})),re=function(t){return u(Kt,Jt,m,t)},oe=32,ue=r((function(t,e,n,a){return{$:0,a:t,b:e,c:n,d:a}})),ie=[],le=x,fe=n((function(t,e){return C(e)/C(t)})),ce=le(o(fe,2,oe)),de=i(ue,0,ce,ie,ie),ye=L,he=p,me=function(t){return t.length},ve=n((function(t,e){return c(t,e)>0?t:e})),se=g,be=n((function(t,e){for(;;){var n=o(se,oe,t),a=n.b,r=o(Jt,{$:0,a:n.a},e);if(!a.b)return re(r);t=a,e=r}})),Ne=n((function(t,e){for(;;){var n=le(e/oe);if(1===n)return o(se,oe,t).a;t=o(be,t,m),e=n}})),Le=n((function(t,e){if(e.b){var n=e.b*oe,a=he(o(fe,oe,n-1)),r=t?re(e.e):e.e,u=o(Ne,r,e.b);return i(ue,me(e.d)+n,o(ve,5,a*ce),u,e.d)}return i(ue,me(e.d),ce,ie,e.d)})),ge=function(t){return e(5,t,(function(e){return function(n){return function(a){return function(r){return function(o){return t(e,n,a,r,o)}}}}}))}((function(t,e,n,a,r){for(;;){if(0>e)return o(Le,!1,{e:a,b:n/oe|0,d:r});var i={$:1,a:u(ye,oe,e,t)};e-=oe,a=o(Jt,i,a)}})),we=n((function(t,e){if(t>0){var n=t%oe;return function(t,e,n,a,r,o){return 5===t.a?t.f(e,n,a,r,o):t(e)(n)(a)(r)(o)}(ge,e,t-n-oe,t,m,u(ye,n,t-n,e))}return de})),xe=function(t){return!t.$},pe=O,Ce=S,$e=function(t){return{$:0,a:t}},je=function(t){switch(t.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},_e=function(t){return t},Me=T,Oe=Me(0),Se=r((function(t,e,n,a){if(a.b){var r=a.a,l=a.b;if(l.b){var f=l.a,c=l.b;if(c.b){var d=c.a,y=c.b;if(y.b){var h=y.b;return o(t,r,o(t,f,o(t,d,o(t,y.a,n>500?u(Kt,t,e,re(h)):i(Se,t,e,n+1,h)))))}return o(t,r,o(t,f,o(t,d,e)))}return o(t,r,o(t,f,e))}return o(t,r,e)}return e})),Fe=a((function(t,e,n){return i(Se,t,e,0,n)})),ke=n((function(t,e){return u(Fe,n((function(e,n){return o(Jt,t(e),n)})),m,e)})),Ae=q,ze=n((function(t,e){return o(Ae,(function(e){return Me(t(e))}),e)})),Ee=a((function(t,e,n){return o(Ae,(function(e){return o(Ae,(function(n){return Me(o(t,e,n))}),n)}),e)})),We=U,He=n((function(t,e){var n=e;return function(t){return J((function(e){e(T(V(t)))}))}(o(Ae,We(t),n))}));Z.Task={b:Oe,c:a((function(t,e){var n;return o(ze,(function(){return 0}),(n=o(ke,He(t),e),u(Fe,Ee(Jt),Me(m),n)))})),d:a((function(){return Me(0)})),e:n((function(t,e){return o(ze,t,e)})),f:void 0};var Ie,Te=It,Je=G(m),qe=d(0,Je),Be=G(m),Ve=n((function(t,e){return d(e,Je)})),Xe=function(t){return y(t,{ax:2})},Pe=n((function(t,e){return o(lt,function(t){return/^(on|formAction$)/i.test(t)?"data-"+t:t}(t),function(t){return/^\s*(javascript:|data:text\/html)/i.test(t)?"":t}(e))})),Ye=function(t){return{m:t,o:m}},Qe=n((function(t,e){return Ye(b([o(Pe,t,e)]))})),Ze=_e,Re=Ze({av:.6,aN:.6}),De=n((function(t,e){return o(it,t,e)}))("className"),Ue=function(t){return Ye(b([De(t)]))},Ge=n((function(t,e){return{$:0,a:t,b:e}})),Ke=function(t){return{$:2,a:t}},tn={$:4},en={m:m,o:m},nn=n((function(t,e){return{F:e,j:t,H:0,h:en,a:"div",J:t}})),an={av:0,aN:0},rn=function(t){switch(t.$){case 0:return t.a;case 1:case 2:case 3:return t.a.J;default:return an}},on=a((function(t,e,n){var a=t(e);return a.$?n:o(Jt,a.a,n)})),un=n((function(t,e){return u(Fe,on(t),m,e)})),ln=n((function(t,e){var n=t.ax,a=o(un,(function(t){var e=t;return l(e.s,tn)?Dt:Rt(e)}),e);if(a.b){var r=a.a,u=a.b,i=rn(r.s);return Ke(y(o(nn,i,o(Ge,r,u)),{H:n}))}return tn})),fn=n((function(t,e){return{W:3,s:e,X:!1,I:t}})),cn={ax:0},dn={ax:0,ao:!1},yn=function(t){return{$:1,a:t}},hn={$:0},mn={$:0},vn=n((function(t,e){return{$:1,a:t,b:e}})),sn=n((function(t,e){return{$:1,a:t,b:e}})),bn={$:4},Nn=y({s:bn,ae:!1,N:0,af:!1,Y:hn,Z:mn,_:o(vn,"",0),aa:o(sn,"",0),h:en,a:"div",ab:m,Q:{av:0,aN:0},ac:!1,V:0},{s:yn(tn)}),Ln=function(t){return y(t,{ac:!0})},gn=n((function(t,e){return{$:0,a:t,b:e}})),wn=function(t){return{$:1,a:t}},xn=function(t){return{$:3,a:t}},pn=n((function(t,e){var n=t,a=e;switch(a.$){case 0:return o(gn,n,a.b);case 1:return wn(y(a.a,{J:n}));case 2:return Ke(y(a.a,{J:n}));case 3:return xn(y(a.a,{J:n}));default:return tn}})),Cn={h:en,a:"span",aM:""},$n=function(t){return""===t?Cn:{h:en,a:"span",aM:t}},jn=n((function(t,e){return u(Fe,n((function(e,n){return t(e)?o(Jt,e,n):n})),m,e)})),_n=n((function(t,e){var n=t,a=o(jn,(function(t){return""!==t.aM}),e);return a.b?xn({j:n,h:en,a:"div",J:n,an:d(a.a,a.b)}):tn})),Mn=n((function(t,e){return{W:1,s:e,X:!0,I:t}})),On=n((function(t,e){return{W:3,s:e,X:!0,I:t}})),Sn=n((function(t,e){return{W:0,s:e,X:!0,I:t}})),Fn=n((function(t,e){return{W:1,s:e,X:!0,I:t}})),kn=n((function(t,e){return{W:3,s:e,X:!0,I:t}})),An=n((function(t,e){return{W:1,s:e,X:!1,I:t}})),zn={av:0,aN:0},En=zn,Wn=dt,Hn=n((function(t,e){var n=e;return{m:o(ke,Wn(t),n.m),o:n.o}})),In=n((function(t,e){return{h:o(Hn,t,e.h),a:e.a,aM:e.aM}})),Tn=n((function(t,e){var n=e.b,a=function(e){return{W:e.W,s:t(e.s),X:e.X,I:e.I}};return o(Ge,a(e.a),o(ke,a,n))})),Jn=n((function(t,e){return o(qn,t,e)})),qn=n((function(t,e){return{s:function(){var n=e.s;switch(n.$){case 0:var a=n.a;return{$:0,a:o(ke,In(t),a)};case 1:return yn(o(Vn,t,n.a));case 2:return{$:2,a:o(ke,(function(e){return d(e.a,o(qn,t,e.b))}),n.a)};case 3:return function(t){return{$:3,a:t}}(n.a);default:return bn}}(),ae:e.ae,N:e.N,af:e.af,Y:e.Y,Z:e.Z,_:e._,aa:e.aa,h:o(Hn,t,e.h),a:e.a,ab:o(Bn,t,e.ab),Q:e.Q,ac:e.ac,V:e.V}})),Bn=function(t){return ke((function(e){return{A:e.A,ad:o(Jn,t,e.ad),ah:e.ah}}))},Vn=n((function(t,e){switch(e.$){case 0:return o(gn,e.a,o(qn,t,e.b));case 1:var n=e.a;return wn({F:o(Tn,Vn(t),n.F),j:n.j,H:n.H,h:o(Hn,t,n.h),a:n.a,J:n.J,ao:n.ao});case 2:return n=e.a,Ke({F:o(Tn,Vn(t),n.F),j:n.j,H:n.H,h:o(Hn,t,n.h),a:n.a,J:n.J});case 3:return xn({j:(n=e.a).j,h:o(Hn,t,n.h),a:n.a,J:n.J,an:(a=n.an,r=a.b,d(o(In,t,a.a),o(ke,In(t),r)))});default:return tn}var a,r})),Xn=a((function(t,e,n){var a=n;return y(a,{ab:o(Jt,{A:e.a,ad:o(Jn,(function(t){return t}),e.b),ah:t},a.ab)})})),Pn=n((function(t,e){return{F:e,j:t,H:0,h:en,a:"div",J:t,ao:!0}})),Yn=n((function(t,e){var n=t.ax,a=t.ao,r=o(un,(function(t){var e=t;return l(e.s,tn)?Dt:Rt(e)}),e);if(r.b){var u=r.a,i=r.b,f=rn(u.s);return wn(y(o(Pn,f,o(Ge,u,i)),{H:n,ao:a}))}return tn})),Qn={s:bn,ae:!1,N:0,af:!1,Y:hn,Z:mn,_:o(vn,"",0),aa:o(sn,"",0),h:en,a:"div",ab:m,Q:an,ac:!1,V:0},Zn=function(t){var e=t;return y(Qn,{s:l(e,tn)?bn:yn(e),Q:rn(e)})},Rn=a((function(t,e,n){return t(e(n))})),Dn={m:m,o:m},Un=o(Kt,n((function(t,e){var n=t,a=e;return{m:h(n.m,a.m),o:h(n.o,a.o)}})),Dn),Gn=o(Rn,n((function(t,e){return y(e,{h:Un(b([e.h,t]))})})),Ue),Kn=n((function(t,e){return 4===e.s.$?tn:o(gn,t,e)})),ta=n((function(t,e){return{$:2,a:t,b:e}})),ea=o(Rn,n((function(t,e){return y(e,{N:1,Y:t})})),ta("em")),na=n((function(t,e){return{$:2,a:t,b:e}})),aa=o(Rn,n((function(t,e){return y(e,{Z:t,V:1})})),na("em")),ra=o(Rn,n((function(t,e){return y(e,{_:t})})),vn("em")),oa=o(Rn,n((function(t,e){return y(e,{aa:t})})),sn("em")),ua=n((function(t,e){return y(e,{h:Un(b([e.h,t]))})})),ia=n((function(t,e){return y(e,{a:t})})),la=n((function(t,e){var n=e;switch(n.$){case 0:return o(gn,n.a,y(n.b,{a:t}));case 1:return wn(y(n.a,{a:t}));case 2:return Ke(y(n.a,{a:t}));case 3:return xn(y(n.a,{a:t}));default:return tn}})),fa=n((function(t,e){return{m:m,o:b([d(t,e)])}})),ca=Ze({av:.3,aN:.3}),da=n((function(t,e){return o(_n,t,b([$n(e)]))})),ya=function(t){return o(Jn,_e,y(t,{ae:!0}))},ha=n((function(t,e){return{$:0,a:t,b:e}})),ma={f:o(ha,"rem",1),aW:!1},va=function(t){return Ue("elmNeatLayout--"+t)},sa=rt("div"),ba=n((function(t,e){var a;return t((a=Un(e),o(Jt,o(Pe,"style",u(Kt,n((function(t,e){return e+(t.a+":")+t.b+";"})),"",a.o)),a.m)))})),Na=ba(sa),La=function(t){return ba(rt(ft(t)))},ga=j,wa=n((function(t,e){return o(ha,e.a,t(e.b))})),xa=function(t){return wa((function(e){return t*e}))},pa=function(t){var e,n=t.a;return e=b([ga(t.b),n]),o(Gt,"",e)},Ca=a((function(t,e,n){var a=e.P;return Un(b([o(fa,"--outer-gap-x",pa(o(xa,a.av,t.f))),o(fa,"--outer-gap-y",pa(o(xa,a.aN,t.f))),o(fa,"--inner-gap-x",pa(o(xa,n.Q.av,t.f))),o(fa,"--inner-gap-y",pa(o(xa,n.Q.aN,t.f))),function(){var e=n.aa;if(e.$){var a=e.a;return o(fa,"--min-width",h(ga(e.b),a))}return o(fa,"--min-width",pa(o(xa,e.a,t.f)))}(),function(){var e=n.Z;switch(e.$){case 1:return o(fa,"--max-width",pa(o(xa,e.a,t.f)));case 2:var a=e.a;return o(fa,"--max-width",h(ga(e.b),a));default:return en}}(),function(){var e=n._;if(e.$){var a=e.a;return o(fa,"--min-height",h(ga(e.b),a))}return o(fa,"--min-height",pa(o(xa,e.a,t.f)))}(),function(){var e=n.Y;switch(e.$){case 1:return o(fa,"--max-height",pa(o(xa,e.a,t.f)));case 2:var a=e.a;return o(fa,"--max-height",h(ga(e.b),a));default:return en}}()]))})),$a=function(t){switch(t.$){case 1:case 2:return!0;default:return!1}},ja=function(t){switch(t.$){case 1:case 2:return!0;default:return!1}},_a=function(t){return ba(ot(ft(t)))},Ma=n((function(t,e){return e.$?Dt:Rt(t(e.a))})),Oa=at,Sa=a((function(t,e,n){var a,r=Un(b([n.h,e.r,va("textView"),o(fa,"--content-gap-x",pa(o(xa,n.j.av,t.f))),o(fa,"--content-gap-y",pa(o(xa,n.j.aN,t.f)))]));return u(La,n.a,b([r]),b([o(Na,b([va("textView_textMargin")]),o(ke,(function(t){return u(La,t.a,b([t.h,va("textView_inline")]),b([Oa(t.aM)]))}),(a=n.an,o(Jt,a.a,a.b))))]))})),Fa=n((function(t,e){return e.$?t:e.a})),ka=r((function(t,e,a,r){var f,c,y,m=e.r,v=re(r.ab),s={t:Un(b([va(r.N?"heightFlex":"heightMinSize"),va(r.V?"widthFlex":"widthMinSize")])),r:va("boundaryContent")},N=Un(b([r.h,u(Ca,t,a,r),s.t,m,va("boundary"),r.af?va("boundary-horizontalOverflow"):en,r.ac?va("boundary-verticalOverflow"):en,$a(r.Y)?va("boundary-hasMaxHeight"):en,ja(r.Z)?va("boundary-hasMaxWidth"):en,(y=r._,(y.$?y.b:y.a)?va("boundary-hasMinHeight"):en),(c=r.aa,(c.$?c.b:c.a)?va("boundary-hasMinWidth"):en),(f=r.ab,f.b?va("boundary-hasOverlays"):en),r.ae?va("boundary-enforcePointerEvent"):en])),L=r.s;switch(L.$){case 4:return Oa("");case 1:var g=L.a;return r.ac&&r.Q.aN?u(La,r.a,b([N]),b([u(_a,"div",b([va("boundary_scroller"),va("boundary_scroller-verticalScroll")]),o(Jt,d("content",u(Wa,t,s,g)),o(ke,za(t),v)))])):l(g,tn)?u(_a,r.a,b([N,va("boundary-view"),va("boundary-view-noContent")]),o(ke,za(t),v)):u(_a,r.a,b([N,va("boundary-view"),va("boundary-view-hasContent")]),o(Jt,d("content",u(Wa,t,s,g)),o(ke,za(t),v)));case 2:var w=L.a;return u(_a,r.a,b([N,va("boundary-html")]),h(o(ke,(function(e){return d(e.a,i(ka,t,s,{P:zn},e.b))}),w),o(ke,za(t),v)));case 0:var x=L.a;return u(La,r.a,b([N,va("boundary-text")]),b([u(_a,"div",b([va("boundary_textMargin")]),function(e){return h(e,o(ke,za(t),v))}(o(ae,n((function(t,e){return d("content"+Ut(t),u(La,e.a,b([e.h,va("boundary_text")]),b([Oa(e.aM)])))})),x)))]));default:var p=L.a;return u(La,r.a,b([N]),b([Oa(p)]))}})),Aa=a((function(t,e,n){var a=e.t,r=function(t){return{t:a,r:Un(b([va("columnChild"),t.X?va("columnChild-grow"):en,function(){switch(t.W){case 0:return va("columnChild-alignStart");case 1:return va("columnChild-alignCenter");case 2:return va("columnChild-alignEnd");default:return va("columnChild-alignStretch")}}()]))}},i=Un(b([n.h,e.r,va("column"),function(){switch(n.H){case 0:return va("column-justifyStart");case 1:return va("column-justifyCenter");case 2:return va("column-justifyEnd");default:return va("column-justifyStretch")}}(),o(fa,"--content-gap-x",pa(o(xa,n.j.av,t.f))),o(fa,"--content-gap-y",pa(o(xa,n.j.aN,t.f)))])),l=n.F;if(l.b.b){var f=l.a,c=l.b;return u(_a,n.a,b([i,va("column-multi")]),o(ke,(function(e){return d(e.I,u(Wa,t,r(e),e.s))}),o(Jt,f,c)))}var y=l.a;return u(_a,n.a,b([i,va("column-single")]),b([d(y.I,u(Wa,t,r(y),y.s))]))})),za=n((function(t,e){var n={t:Un(b([va("heightFlex"),va("widthFlex")])),r:Un(b([va("overlay"),o(fa,"--overlay-top",ga(e.A.ba)+"%"),o(fa,"--overlay-bottom",ga(e.A.aU)+"%"),o(fa,"--overlay-left",ga(e.A.a1)+"%"),o(fa,"--overlay-right",ga(e.A.a7)+"%"),o(fa,"--overlay-priority",o(Fa,"auto",o(Ma,Ut,e.A.a5)))]))};return d("overlay-"+e.ah,i(ka,t,n,{P:zn},y(e.ad,{N:1,V:1})))})),Ea=a((function(t,e,n){var a=e.t,r=function(t){return{t:a,r:Un(b([va("rowChild"),t.X?va("rowChild-grow"):en,function(){switch(t.W){case 0:return va("rowChild-alignStart");case 1:return va("rowChild-alignCenter");case 2:return va("rowChild-alignEnd");default:return va("rowChild-alignStretch")}}()]))}},i=Un(b([n.h,e.r,va("row"),n.ao?va("row-wrap"):en,function(){switch(n.H){case 0:return va("row-justifyStart");case 1:return va("row-justifyCenter");case 2:return va("row-justifyEnd");default:return va("row-justifyStretch")}}(),o(fa,"--content-gap-x",pa(o(xa,n.j.av,t.f))),o(fa,"--content-gap-y",pa(o(xa,n.j.aN,t.f)))])),l=n.F;if(l.b.b){var f=l.a,c=l.b;return u(_a,n.a,b([i,va("row-multi")]),o(ke,(function(e){return d(e.I,u(Wa,t,r(e),e.s))}),o(Jt,f,c)))}var y=l.a;return u(_a,n.a,b([i,va("row-single")]),b([d(y.I,u(Wa,t,r(y),y.s))]))})),Wa=a((function(t,e,n){switch(n.$){case 0:return i(ka,t,e,{P:n.a},n.b);case 1:return u(Ea,t,e,n.a);case 2:return u(Aa,t,e,n.a);case 3:return u(Sa,t,e,n.a);default:return Oa("")}})),Ha=n((function(t,e){var n=t,a=e,r={t:Un(b([va("heightFlex"),va("widthFlex")])),r:va("top")};return o(Na,m,b([u(La,"style",b([o(fa,"display","none")]),b([Oa('.elmNeatLayout,.elmNeatLayout:before,.elmNeatLayout:after{box-sizing:border-box;margin:0;padding:0}.elmNeatLayout--top{display:block;position:fixed;inset:0;overflow:hidden}.elmNeatLayout--overlay{pointer-events:none;top:var(--overlay-top);bottom:var(--overlay-bottom);left:var(--overlay-left);right:var(--overlay-right);z-index:var(--overlay-priority);display:block;position:absolute;overflow:hidden}.elmNeatLayout--boundary{overflow:hidden}.elmNeatLayout--boundary-hasOverlays:not(.elmNeatLayout--top){position:relative}.elmNeatLayout--boundary-enforcePointerEvent{pointer-events:auto}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller{height:100%;width:100%}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller-verticalScroll>.elmNeatLayout--boundaryContent{height:auto;min-height:100%}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller-horizontalScroll>.elmNeatLayout--boundaryContent{width:auto;min-width:100%}.elmNeatLayout--boundary-view-hasContent,.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller{padding:var(--inner-gap-y)var(--inner-gap-x)}.elmNeatLayout--boundary-text{overflow:visible}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin{height:auto;width:100%}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin:before{width:0;height:0;margin-top:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin:after{width:0;height:0;margin-bottom:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin>.elmNeatLayout--boundary_text{line-height:calc(1em + var(--outer-gap-y));display:inline}.elmNeatLayout--boundary-horizontalOverflow,.elmNeatLayout--boundary-horizontalOverflow>.elmNeatLayout--boundary_scroller{overflow-x:auto}.elmNeatLayout--boundary-verticalOverflow,.elmNeatLayout--boundary-verticalOverflow>.elmNeatLayout--boundary_scroller{overflow-y:auto}.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin{overflow-y:hidden}.elmNeatLayout--boundary-verticalOverflow>.elmNeatLayout--boundaryContent.elmNeatLayout--column{height:auto}.elmNeatLayout--boundary-hasMinHeight{min-height:var(--min-height)}.elmNeatLayout--boundary-hasMaxHeight:not(.elmNeatLayout--boundary-verticalOverflow){max-height:var(--max-height)}.elmNeatLayout--boundary-hasMinWidth{min-width:var(--min-width)}.elmNeatLayout--boundary-hasMaxWidth:not(.elmNeatLayout--boundary-horizontalOverflow){max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--rowChild{flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild:not(.elmNeatLayout--boundary-horizontalOverflow){width:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex:not(.elmNeatLayout--boundary-verticalOverflow){height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex:not(.elmNeatLayout--boundary-verticalOverflow).elmNeatLayout--rowChild-alignStretch,.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex.elmNeatLayout--boundary-hasMaxHeight{max-height:var(--max-height)}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightMinSize{height:auto;max-height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightMinSize.elmNeatLayout--boundary-hasMaxHeight{max-height:min(var(--max-height),100%)}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-horizontalOverflow{width:0;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--boundary-hasMaxWidth{max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--columnChild{flex-shrink:0}.elmNeatLayout--boundary.elmNeatLayout--columnChild:not(.elmNeatLayout--boundary-verticalOverflow){height:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex:not(.elmNeatLayout--boundary-horizontalOverflow){width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex:not(.elmNeatLayout--boundary-horizontalOverflow).elmNeatLayout--columnChild-alignStretch,.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex.elmNeatLayout--boundary-hasMaxWidth{max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthMinSize{width:auto;max-width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthMinSize.elmNeatLayout--boundary-hasMaxWidth{max-width:min(var(--max-width),100%)}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-verticalOverflow{height:0;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-hasMaxHeight{max-height:var(--max-height)}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-verticalOverflow{height:auto;max-height:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-hasMaxHeight{max-height:min(var(--max-height),100%)}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-horizontalOverflow{width:auto;max-width:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--boundary-hasMaxWidth{max-width:min(var(--max-width),100%)}.elmNeatLayout--row{gap:var(--content-gap-y)var(--content-gap-x);flex-flow:row;display:flex}.elmNeatLayout--row.elmNeatLayout--row-wrap{flex-wrap:wrap}.elmNeatLayout--row.elmNeatLayout--row-justifyStart{justify-content:flex-start}.elmNeatLayout--row.elmNeatLayout--row-justifyCenter{justify-content:center}.elmNeatLayout--row.elmNeatLayout--row-justifyEnd{justify-content:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild{flex-grow:0}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-grow{flex-grow:1}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStart{align-self:flex-start}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignCenter{align-self:center}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignEnd{align-self:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{align-self:stretch}.elmNeatLayout--row.elmNeatLayout--rowChild{width:auto;height:auto;flex-shrink:1}.elmNeatLayout--row.elmNeatLayout--columnChild{height:auto;width:100%;flex-shrink:0}.elmNeatLayout--row.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--row.elmNeatLayout--boundaryContent{width:100%;height:100%}.elmNeatLayout--column{gap:var(--content-gap-y)var(--content-gap-x);flex-flow:column;display:flex}.elmNeatLayout--column.elmNeatLayout--column-justifyStart{justify-content:flex-start}.elmNeatLayout--column.elmNeatLayout--column-justifyCenter{justify-content:center}.elmNeatLayout--column.elmNeatLayout--column-justifyEnd{justify-content:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild{flex-grow:0}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-grow{flex-grow:1}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStart{align-self:flex-start}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignCenter{align-self:center}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignEnd{align-self:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{align-self:stretch}.elmNeatLayout--column.elmNeatLayout--rowChild{width:auto;height:100%;flex-shrink:1}.elmNeatLayout--column.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--column.elmNeatLayout--columnChild{height:auto;width:auto;flex-shrink:0}.elmNeatLayout--column.elmNeatLayout--boundaryContent{width:100%;height:100%}')])),i(ka,n,r,{P:zn},y(a,{N:1,V:1}))]))}));Ie={Main:{init:Te({a0:function(){return qe},a8:function(){return Be},bb:Ve,bc:function(){var t;return{aT:b([o(Ha,ma,Zn(o(ln,y(cn,{ax:2}),b([o(fn,"header",o(Kn,En,o(ua,Ue("header"),Zn(o(Yn,dn,b([o(Fn,"text",o(da,Re,"Header")),o(Fn,"input",o(la,"input",o(Kn,Re,o(ua,o(fa,"padding","0.4em"),o(ua,o(Qe,"placeholder","foo"),o(oa,4,Nn)))))),o(An,"hamburger",o(Kn,Re,o(ua,Ue("header_hamburger"),o(ea,3,o(ra,3,o(aa,3,o(oa,3,Zn(o(Yn,(t=dn,y(t,{ax:1})),b([o(An,"icon",o(da,En,"三"))]))))))))))])))))),o(On,"body",o(Kn,En,o(ua,Ue("blue"),Ln(o(ra,10,Zn(o(ln,cn,b([o(fn,"sampleText",o(pn,Re,o(_n,ca,b([$n("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do "),o(ia,"code",o(Gn,"inlineCode",$n("<eiusmod>"))),$n(" tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")])))),o(fn,"subBoxes",o(pn,Re,o(ln,cn,b([o(fn,"sampleBox1",o(Kn,ca,o(ua,Ue("red"),o(aa,20,o(oa,10,o(ra,2,Nn)))))),o(fn,"sampleBox2",o(Kn,ca,o(ua,Ue("blue"),o(oa,7,o(ra,2,Nn)))))])))),o(Mn,"sampleNestedBox",o(Kn,Re,u(Xn,"overlay",d({aU:0,a1:0,a5:Dt,a7:50,ba:50},ya(o(ua,Ue("red"),o(aa,20,Nn)))),o(ea,33,o(aa,40,o(oa,37,o(ua,Ue("red"),Zn(o(Yn,Xe(dn),b([o(kn,"sampleBox",o(Kn,Re,o(ua,Ue("blue"),o(aa,23,o(oa,12,o(ra,5,Nn))))))])))))))))),o(Sn,"content1",o(Kn,Re,o(ua,Ue("scrollableText_content"),o(ea,8,o(ra,1,Ln(Zn(o(Yn,dn,b([o(kn,"content",o(da,ca,"foo000000\nbar\nbar\nbar\nfoo\nbar\nbar\nbar\n"))]))))))))),o(On,"content2",o(Kn,Re,o(ua,Ue("scrollableText_content"),o(ea,8,o(ra,1,Ln(Zn(o(pn,Re,o(Yn,dn,b([o(kn,"content",o(da,ca,"foo000000\nbar\nbar\nbar"))]))))))))))])))))))),o(fn,"footer",o(Kn,En,o(ua,Ue("footer"),Zn(o(da,Re,"Footer")))))]))))]),a9:"Sample"}}})($e(0))(0)}},t.Elm?function t(e,n){for(var a in n)a in e?"init"==a?w(6):t(e[a],n[a]):e[a]=n[a]}(t.Elm,Ie):t.Elm=Ie}(t);t.Elm.Main.init({node:document.body.appendChild(document.createElement("div")),flags:null});