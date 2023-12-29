export const stackSafe = (f) => (v) => () => {
    setImmediate(() => f(v)());
};