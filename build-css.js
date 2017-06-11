const fs       = require('fs');
const postcss  = require('postcss');
const atimport = require('postcss-import');
const modscale = require('postcss-modular-scale');
const cssprops = require('postcss-custom-properties');
const clrfunc  = require('postcss-color-function');
const cssnano  = require('cssnano');
const autoprefixer = require('autoprefixer');

const processor = postcss([
  atimport,
  modscale,
  cssprops,
  clrfunc,
  cssnano,
  autoprefixer
]);

const src  = './style.css';
const dest = './dist/style.min.css';

const css = fs.readFileSync(src);

processor.process(css, {
  from : src,
  to   : dest,
  map  : { inline : false }
}).then(result => {
  result.warnings().forEach(warn => console.error(warn));
  fs.writeFileSync(dest, result.css);
  console.log(`Wrote ${dest}`);
  fs.writeFileSync(`${dest}.map`, result.map);
  console.log(`Wrote ${dest}.map`);
});
