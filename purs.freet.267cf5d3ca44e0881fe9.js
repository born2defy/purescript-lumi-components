(window.webpackJsonp=window.webpackJsonp||[]).push([[104],{Y6Zo:function(n,t,r){"use strict";var e=r("UCB5"),u=r("v0Xu"),o=r("Z7LG"),i=r("4Bm9"),c=r("IGzz"),a=r("wUcd"),f=r("AqgI"),l=r("J9YH"),p=r("XTTY"),d=r("DGTj"),s=r("qA++"),m=r("t7hT"),w=r("t5nC"),v=r("TFw3"),F=r("sAO9"),M=r("T4xb"),T=r("KGqT"),h=r("AKjc"),E=r("3oCz"),A=function(){function n(n,t){this.value0=n,this.value1=t}return n.create=function(t){return function(r){return new n(t,r)}},n}(),b=function(){function n(n){this.value0=n}return n.create=function(t){return new n(t)},n}(),y=function(){function n(n){this.value0=n}return n.create=function(t){return new n(t)},n}(),g=function(n){return new d.MonadTrans(function(n){return function(t){return new b(function(r){return F.map(n.Bind1().Apply0().Functor0())(w.Left.create)(t)})}})},C=b.create,B=function(n){return function(t){return new y(v.mkExists(new A(n,t)))}},L=function(n){return function(t){return new F.Functor(function(r){return function(e){if(e instanceof b)return new b(function(u){return F.map(t)(m.bimap(w.bifunctorEither)(r)(F.map(n)(F.map(L(n)(t))(r))))(e.value0(h.unit))});if(e instanceof y)return v.runExists(function(e){return B(e.value0)(function(u){return F.map(L(n)(t))(r)(e.value1(u))})})(e.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 57, column 1 - line 57, column 71): "+[r.constructor.name,e.constructor.name])}})}},x=function(n){return function(t){return function(r){return function(e){return function(u){if(u instanceof y)return v.runExists(function(u){return B(function(o){return x(n)(t)(r)(e)(u.value0(o))})(function(o){return x(n)(t)(r)(e)(u.value1(o))})})(u.value0);if(u instanceof b)return new b(function(o){return F.map(t)(F.map(w.functorEither)(function(u){return r(F.map(n)(x(n)(t)(r)(e))(u))}))(e(u.value0(h.unit)))});throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 118, column 1 - line 118, column 109): "+[r.constructor.name,e.constructor.name,u.constructor.name])}}}}},R=function(n){return function(t){return new c.Monad(function(){return G(n)(t)},function(){return k(n)(t)})}},k=function(n){return function(t){return new o.Bind(function(){return D(n)(t)},function(n){return function(t){return n instanceof y?v.runExists(function(n){return B(n.value0)(function(r){return B(function(t){return n.value1(r)})(t)})})(n.value0):B(function(t){return n})(t)}})}},D=function(n){return function(t){return new u.Apply(function(){return L(n)(t.Bind1().Apply0().Functor0())},c.ap(R(n)(t)))}},G=function(n){return function(t){return new e.Applicative(function(){return D(n)(t)},function(n){return new b(function(r){return e.pure(t.Applicative0())(new w.Left(n))})})}},S=function(n){return function(t){return l.tailRecM(t)(function(r){if(r instanceof b)return F.map(t.Monad0().Bind1().Apply0().Functor0())(l.Done.create)(r.value0(h.unit));if(r instanceof y)return v.runExists(function(r){var u=r.value0(h.unit);if(u instanceof b)return o.bind(t.Monad0().Bind1())(u.value0(h.unit))(function(u){if(u instanceof w.Left)return e.pure(t.Monad0().Applicative0())(new l.Loop(r.value1(u.value0)));if(u instanceof w.Right)return e.pure(t.Monad0().Applicative0())(new l.Done(new w.Right(F.map(n)(function(e){return o.bind(k(n)(t.Monad0()))(e)(r.value1)})(u.value0))));throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 52, column 20 - line 54, column 67): "+[u.constructor.name])});if(u instanceof y)return v.runExists(function(u){return e.pure(t.Monad0().Applicative0())(new l.Loop(o.bind(k(n)(t.Monad0()))(u.value0(h.unit))(function(e){return o.bind(k(n)(t.Monad0()))(u.value1(e))(r.value1)})))})(u.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 50, column 5 - line 55, column 98): "+[u.constructor.name])})(r.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 47, column 3 - line 47, column 75): "+[r.constructor.name])})}},q=function(n){return function(t){return function(r){return new T.Semigroup(u.lift2(D(n)(t))(T.append(r)))}}};n.exports={freeT:C,liftFreeT:function(n){return function(t){return function(r){return new b(function(u){return e.pure(t.Applicative0())(new w.Right(F.map(n)(e.pure(G(n)(t)))(r)))})}}},hoistFreeT:function(n){return function(t){return x(n)(t)(i.identity(i.categoryFn))}},interpret:function(n){return function(t){return function(r){return x(n)(t)(r)(i.identity(i.categoryFn))}}},bimapFreeT:x,resume:S,runFreeT:function(n){return function(t){return function(r){return l.tailRecM(t)(o.composeKleisliFlipped(t.Monad0().Bind1())(function(n){if(n instanceof w.Left)return e.pure(t.Monad0().Applicative0())(new l.Done(n.value0));if(n instanceof w.Right)return F.map(t.Monad0().Bind1().Apply0().Functor0())(l.Loop.create)(r(n.value0));throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 126, column 3 - line 126, column 63): "+[n.constructor.name])})(S(n)(t)))}}},functorFreeT:L,applyFreeT:D,applicativeFreeT:G,bindFreeT:k,monadFreeT:R,monadTransFreeT:g,monadRecFreeT:function(n){return function(t){return new l.MonadRec(function(){return R(n)(t)},function(r){var u=function(i){return o.bind(k(n)(t))(r(i))(function(r){if(r instanceof l.Loop)return u(r.value0);if(r instanceof l.Done)return e.pure(G(n)(t))(r.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 80, column 15 - line 82, column 25): "+[r.constructor.name])})};return u})}},semigroupFreeT:q,monoidFreeT:function(n){return function(t){return function(r){return new M.Monoid(function(){return q(n)(t)(r.Semigroup0())},e.pure(G(n)(t))(M.mempty(r)))}}},monadEffectFreeT:function(n){return function(t){return new E.MonadEffect(function(){return R(n)(t.Monad0())},function(n){return d.lift(g())(t.Monad0())(E.liftEffect(t)(n))})}},monadAskFreeT:function(n){return function(t){return new f.MonadAsk(function(){return R(n)(t.Monad0())},d.lift(g())(t.Monad0())(f.ask(t)))}},monadTellFreeT:function(n){return function(t){return new s.MonadTell(function(){return R(n)(t.Monad0())},function(n){return d.lift(g())(t.Monad0())(s.tell(t)(n))})}},monadStateFreeT:function(n){return function(t){return new p.MonadState(function(){return R(n)(t.Monad0())},function(n){return d.lift(g())(t.Monad0())(p.state(t)(n))})}},monadThrowFreeT:function(n){return function(t){return new a.MonadThrow(function(){return R(n)(t.Monad0())},function(n){return d.lift(g())(t.Monad0())(a.throwError(t)(n))})}}};r("sygH")}}]);