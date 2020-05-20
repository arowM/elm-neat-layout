require('../style/reset.scss');
require('../style/app.scss');

require('../style/repl.scss');
require('../style/repl/preview.scss');
require('../style/repl/viewEditor.scss');
// require('../style/repl/gapEditor.scss');

const { Elm } = require('../Main.elm');

const app = Elm.Main.init({
  node: document.body.appendChild(document.createElement('div')),
  flags: null
});
