(window.webpackJsonp=window.webpackJsonp||[]).push([[44],{DNVA:function(t,e,n){"use strict";var a=n("YKFa"),o=n("EVBn"),r=n("Z/IS"),s=n("POE8"),i=n("XSw5"),c=n("ll6l"),u=n("H2/w"),l=n("RqP8"),h=n("aa8G"),p=n("6g2T"),d=n("5L+5"),y=n("bP3h"),m=n("pMgY"),b=n("xhm4"),f=n("7HZk"),g=n("sfHK"),w=n("h+YZ"),v=n("LQUV"),R=f.createComponent("TabExample"),L=i.flip(f.element)({})(b.withRouter(f.toReactComponent()(a.identity(a.categoryFn))(R)({render:function(t){return e=t.props,p.column_([m.h2_("Demo 1"),d.example(w.div()({style:g.css({maxWidth:"800px"}),children:[y.tabs({currentLocation:v.URL("#"+(e.location.pathname+(e.location.search+e.location.hash))),useHash:!0,navigate:new u.Just(function(t){var n=y.urlParts(t),a=n.path+(n.query+(n.hash.path+n.hash.query)),o=u.fromMaybe("")(i.flip(r.index)(1)(h.split("#")(a)));return function(){return e.history.push(v.URL(o))}}),queryKey:"demo1",style:g.css({}),tabStyle:g.css({}),selectedTabStyle:g.css({}),tabs:c.mapFlipped(c.functorArray)(r.range(1)(10))(function(t){var e="Tab with a long title "+l.show(l.showInt)(t);return{content:function(t){return f.empty},id:h.toLower(e),label:e,count:c.voidRight(u.functorMaybe)(7*((7*(t-1|0)|0)*s.mod(s.euclideanRingInt)(t-1|0)(4)|0)|0)(o.guard(u.monadZeroMaybe)(1!==s.mod(s.euclideanRingInt)(t)(3))),testId:u.Nothing.value}})})]})),m.h2_("Demo 2"),d.example(y.tabs({currentLocation:v.URL("#"+(e.location.pathname+(e.location.search+e.location.hash))),useHash:!0,navigate:new u.Just(function(t){var n=y.urlParts(t),a=n.path+(n.query+(n.hash.path+n.hash.query)),o=u.fromMaybe("")(i.flip(r.index)(1)(h.split("#")(a)));return function(){return e.history.push(v.URL(o))}}),queryKey:"demo2",style:g.css({}),tabStyle:g.css({}),selectedTabStyle:g.css({}),tabs:c.mapFlipped(c.functorArray)(r.range(1)(6))(function(t){var e="Tab"+l.show(l.showInt)(t);return{content:function(t){return f.empty},id:h.toLower(e),label:e,count:c.voidRight(u.functorMaybe)(s.div(s.euclideanRingInt)(t)(2))(o.guard(u.monadZeroMaybe)(1===s.mod(s.euclideanRingInt)(t)(4))),testId:u.Nothing.value}})}))]);var e}})));t.exports={component:R,docs:L}}}]);