export const ffiLaunchPFiber = (f) => () => [
    new Promise(r => f(a => () => r(a))())
];

export const ffiWaitPFiber = ([p]) => (f) => () => {
    p.then(a => f(a)());
};
