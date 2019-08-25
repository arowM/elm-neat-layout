require('../style/reset.scss');
require('../style/app.scss');
require('../style/page.scss');

const hljs = require("highlight.js/lib/highlight.js");
hljs.registerLanguage('elm', require('highlight.js/lib/languages/elm.js'));
hljs.registerLanguage('scss', require('highlight.js/lib/languages/scss.js'));
window.hljs = hljs;
require('highlight.js/styles/a11y-dark.css');

const { Elm } = require('../Main.elm');

const app = Elm.Main.init({
  node: document.body,
  flags: null
});

