# Sketch Palette

A utility for generating a sketch palette from a set of colour codes.

## Todo

- [ ] Comprehend sketch palette structure
- [ ] Parse color codes
  - [ ] take hex, hsa, rgb and rgba
  - [ ] be nice about whitespace and octothorpes
  - [ ] be nice about newlines and commas
- [ ] Generate example swatch
- [ ] Provide download link

```javascript
var obj = {a: 123, b: "4 5 6"};
var data = "text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(obj));
```

```json
{
  "colors": [
    {
      "alpha": 1,
      "blue": 0.6078431372549019,
      "green": 0.6078431372549019,
      "red": 0.6078431372549019
    },
    {
      "alpha": 1,
      "blue": 1,
      "green": 1,
      "red": 1
    }
  ],
  "compatibleVersion": "1.4",
  "pluginVersion": "1.4"
}
```
