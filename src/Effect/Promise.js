export const ffiPure = (a) => [Promise.resolve(a)];

export const ffiNew = (f) => () =>
    [new Promise((resolve) => f((a) => () => resolve(a))())];

export const ffiMap = (f) => (p) => [p[0].then(f)];

export const ffiThen = (p) => (f) => () => [p[0].then((a) => f(a)()[0])];

export const ffiAlt = (p1) => (p2) => [
    new Promise((resolve) => {
        p1[0].then(resolve);
        p2[0].then(resolve);
    })
];

export const ffiNever = [new Promise(() => { })];

export const ffiApply = (pF) => (pA) =>
    [Promise.all([pF[0], pA[0]]).then(([f, a]) => f(a))];
