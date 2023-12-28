export const launchPFiber = (f) => () => [
    new Promise(r => f(a => () => r(a))())
];

export const waitPFiber = ([p]) => (f) => () => {
    p.then(a => f(a)());
};
