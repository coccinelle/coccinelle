// TODO: now that have the assign/affect iso, can perhaps
// deal with the cast pb
// rewriting
// - (T*) dev->priv
// + netdev_priv(dev)
// into
// x = 
//  - (T*) dev-> priv
//  + netdev_priv(dev)

// and then make another rule for the remaining case


@ alloc disable plus_comm @
type T;
expression E;
@@

(
(
        alloc_netdev
| 
        alloc_etherdev
|
        alloc_fcdev
|
        alloc_fddidev
|
        alloc_hippi_dev
|
        alloc_trdev
|
        alloc_ltalkdev
|
        alloc_irdadev
| 
        alloc_etherdev_mq
)
   (sizeof(T), ...)
|
(
        alloc_netdev
| 
        alloc_etherdev
|
        alloc_fcdev
|
        alloc_fddidev
|
        alloc_hippi_dev
|
        alloc_trdev
|
        alloc_ltalkdev
|
        alloc_irdadev
| 
        alloc_etherdev_mq
)
   (sizeof(T) + E, ...)
)


// if don't have the iso sizeof(T) => sizeof(E)
// dev = alloc_netdev(sizeof(*x), ...)

//>>> - alloc_irlandev
// but dont pass the sizeof, so cant get the type T.

//> alloc_orinocodev


@ danger @
struct net_device *dev;
expression E;
@@
 dev->priv = E
//+ DANGER


//@ danger @
//struct net_device dev;
//expression E;
//@@
// dev.priv = E



// TODO wrong !!! can have ((T) a)->field,  on peut pas remover le cast!
@ rule1 depends on alloc && !danger  @
struct net_device *dev;
type alloc.T;
@@

- (T*) dev->priv
+ netdev_priv(dev)


// the iso drop_cast is not enough because T* in the previous rule
// can not be dropped. It's not a pure T.
@ rule2 depends on alloc && !danger @
struct net_device *dev;
@@

- dev->priv
+ netdev_priv(dev)
