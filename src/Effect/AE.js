export const makeAEImpl = (f) => () =>
    new Promise(resolve => f(val => () => resolve(val))());

export const runAEImpl = (ep) => (f) => () => {
    ep().then(val => f(val)());
};

// makeAEImpl(runAEImpl(ep))
// = makeAEImpl((f) => () => { ep().then(val => f(val)()); })
// = () => new Promise(resolve => ((f) => () => { ep().then(val => f(val)()); })(val => () => resolve(val))());
// = () => new Promise(resolve => { ep().then(val => (val => () => resolve(val))(val)()); });
// = () => new Promise(resolve => { ep().then(val => resolve(val)); });
// = () => new Promise(resolve => { ep().then(resolve); });
// = () => ep()
// = ep

// runAEImpl(makeAEImpl(f))(g)
// = ((ep) => (h) => () => { ep().then(val => h(val)()); })(makeAEImpl(f))(g)
// = () => { makeAEImpl(f)().then(val => g(val)()); }
// = () => { ((f) => () => new Promise(resolve => f(val => () => resolve(val))()))(f)().then(val => g(val)()); }
// = () => { new Promise(resolve => f(val => () => resolve(val))())).then(val => g(val)()); }
// = () => { new Promise(resolve => f(val => () => resolve(g(val)()))()); }
// = () => { f(val => () => { g(val)(); })(); }
// = f(val => () => { g(val)(); })
// = f(g)

export const delayImpl = (ms) => () => new Promise(resolve => setTimeout(resolve, ms));