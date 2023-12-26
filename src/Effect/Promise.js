export const newPromise = (f) =>
    new Promise(resolve => f(val => () => resolve(val))());