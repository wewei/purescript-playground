export const ffiResolve = Promise.resolve;

export const ffiNew = (f) =>
  new Promise((resolve) => f((val) => () => resolve(val))());

export const ffiThen = (p) => (f) => p.then((val) => f(val)());

export const ffiAlt = (p1) => (p2) =>
  new Promise((resolve) => {
    p1.then(resolve);
    p2.then(resolve);
  });

export const ffiBoth = (tuple) => (pA) => (pB) =>
  Promise.all([pA, pB]).then(([valA, valB]) => tuple(valA)(valB));

export const ffiNever = new Promise(() => {});
