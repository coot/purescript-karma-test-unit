# Karma runner for PureScript [Test.Unit](https://pursuit.purescript.org/packages/purescript-test-unit/10.0.2/docs/Test.Unit) package

Just run `npm test` and you will see PureScript code run inside Karma.

A simple karma configuration:

```javascript
module.exports = config => {
  config.set({
    autoWatch: true,
    browsers: ["Chrome"],
    files: [
      "karma/index.js",
    ],
    reporters: ["spec"],
    singleRun: false
  })
}
```

Then just compile your tests to `karma/index.js` with
```bash
pulp browserify --main 'Test.Main' -I test --to karma/index.js
```
and run the tests with
```bash
karma start --single-run
```

Check the `packcage.json` to see how to automate that with npm scripts.
