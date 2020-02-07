## Instructions

This project uses `Fable.Mocha` to drive learning tests in order for me to play with F#.

The `Fable.Mocha` API is aligned with the API of Expecto. This way I can write my learning tests for both .NET and Javascript. This is possible by sprinkling a few `FABLE_COMPILER` directives in the Tests project to special case the implementation of the runtime.

### First

```
npm install
```

### Then

Run tests compiled by Fable (via `fable-splitter`) in the NodeJS runtime:

```
npm test
```

Run tests compiled by dotnet F# compiler on .NET Core 3.1 runtime:

```
npm run-script test-dotnet
```

Run tests compiled by Fable (via `fable-loader`) in the Browser (The `Fable.Mocha` library will detect if it is running in the browser, and runs the built-in test runner):

```
npm start
```
