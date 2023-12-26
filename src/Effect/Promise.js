export const ffiPure = (a) => Promise.resolve(a);

export const ffiNew = (f) => () =>
  new Promise((resolve) => f((a) => () => resolve(a))());

export const ffiRun = (p) => (f) => () => { p.then((a) => f(a)()) };

export const ffiMap = (f) => (p) => p.then(f);

export const ffiThen = (p) => (f) => () => p.then((a) => f(a)());

export const ffiAlt = (p1) => (p2) =>
  new Promise((resolve) => {
    p1.then(resolve);
    p2.then(resolve);
  });

export const ffiNever = new Promise(() => {});

export const ffiApply = (pF) => (pA) =>
  Promise.all([pF, pA]).then(([f, a]) => f(a));
