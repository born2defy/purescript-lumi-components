(window.webpackJsonp=window.webpackJsonp||[]).push([[14],{"9UDm":function(e,n,t){"use strict";var s,o,r,i=t("JH5H"),c=t("NL7e"),a=t("5a5/"),l=t("W+DG"),u=t("nEj/"),m=t("DkUo"),d=t("JifF"),p=t("jILx"),f=t("R8Br"),b=t("bqfS"),y=t("xEpL"),S=t("eAL6"),g=t("i/4q"),k=t("UYRu"),x=t("7HZk"),v=t("avNA"),_=t("z85V"),C=t("vKy7"),N=t("C9r8"),h=t("bgH5"),w=t("xxbb"),E=function(){function e(){}return e.value=new e,e}(),L=b.styleModifier((function(e){return C.css()({backgroundColor:C.color(i.lighten(.4137)(e.colors.accent2))})})),R=b.styleModifier((function(e){return C.css()({backgroundColor:C.color(e.colors.primary3)})})),B=b.styleModifier((function(e){return C.css()({backgroundColor:C.color(i.lighten(.4137)(e.colors.accent3))})})),D=(s=b.styleModifier((function(e){return C.css()({padding:C.str("8px"),margin:C.str("0 -8px 0 8px"),"&:hover":C.nested(C.css()({color:C.color(e.colors.black1)}))})})),o=function(e){return s(S._interactive(e))},r={component:E.value,content:[],dismissable:!1},u.unsafePerformEffect(m.lumiComponent()()()("Banner")(r)((function(e){return h.bind(h.ixBindRender)(k.useTheme)((function(n){return h.bind(h.ixBindRender)(N.useState(!0))((function(n){return c.pure(h.applicativeRender(w.refl))(l.guard(x.monoidJSX)(n.value0)(m.lumiElement(f.box)(b.styleModifier(e.css)(y.banner((function(t){return{css:t.css,className:e.className,content:[m.lumiElement(f.box)(S._align(S.Stretch.value)(g.onDesktop((s=S._align(S.Center.value),function(e){return s(S._row(e))}))(S._flex((function(n){return{css:n.css,className:n.className,content:e.content,onClick:n.onClick}}))))),l.guard(x.monoidJSX)(e.dismissable)(m.lumiElement(f.box)(S._alignSelf(S.Start.value)(g.onDesktop(S._alignSelf(S.Center.value))(o((function(e){return{css:e.css,className:e.className,content:[d.icon({style:_.css({fontSize:"12px"}),type_:d.Remove.value})],onClick:v.capture_(n.value1((function(e){return!1})))}}))))))],onClick:t.onClick};var s}))))))}))}))})))),M=b.styleModifier((function(e){return C.css()({backgroundColor:C.color(i.lighten(.4137)(e.colors.accent1))})}));e.exports={banner:D,actionBanner:function(e){return function(n){return function(t){return s=n(t),{content:[m.lumiElement(f.box)(S._column(S._flex(g.onDesktop((o=S._align(S.Center.value),function(e){return o(S._row(e))}))((function(n){return{css:n.css,className:n.className,content:[m.lumiElement(f.box)(S._flex((function(e){return{css:e.css,className:e.className,content:s.content,onClick:e.onClick}}))),m.lumiElement(f.box)(S._row(S._align(S.Center.value)(S._justify(S.End.value)(b.styleModifier_(a.fold(a.foldableArray)(C.monoidStyle)([C.css()({margin:C.str("8px 0 0"),"& :not(:first-child)":C.nested(C.css()({marginLeft:C.prop(p.isStylePropertySpace)(p.S8.value)}))}),g.desktopQuery(C.css()({margin:C.str("0 0 0 8px")}))]))((function(n){return{css:n.css,className:n.className,content:e,onClick:n.onClick}}))))))],onClick:n.onClick}})))))],className:s.className,component:s.component,css:s.css,dismissable:s.dismissable};var s,o}}},primary:R,active:M,warning:L,error:B}},"i/4q":function(e,n,t){"use strict";var s=t("YKFa"),o=t("W+DG"),r=t("Qku0"),i=t("bqfS"),c=t("vKy7"),a=function(e){return c.css()({"@media (min-width: 860px)":c.nested(e)})};e.exports={desktopQuery:a,onDesktop:function(e){return i.styleModifier((function(n){return a(e(s.identity(s.categoryFn))(o.mempty(o.monoidRecord()(o.monoidRecordCons(new r.IsSymbol((function(){return"className"})))(o.monoidString)()(o.monoidRecordCons(new r.IsSymbol((function(){return"css"})))(o.monoidFn(c.monoidStyle))()(o.monoidRecordNil))))).css(n))}))}}},qCwT:function(e,n,t){"use strict";var s,o,r,i,c=t("5a5/"),a=t("DkUo"),l=t("SZBU"),u=t("5L+5"),m=t("jILx"),d=t("pMgY"),p=t("9UDm"),f=t("R8Br"),b=t("g4Rk"),y=t("eAL6"),S=t("7HZk"),g=t("sfHK"),k=(s=[g.text("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")],o=[a.lumiElement(f.box)((function(e){return{css:e.css,className:e.className,content:[d.subsectionHeader_("An important title"),g.text("Here's some important message about your account.")],onClick:e.onClick}}))],r=function(e){return[a.lumiElement(p.banner)(y._alignSelf(y.Stretch.value)(b._listSpaced(e))),a.lumiElement(p.banner)(y._alignSelf(y.Stretch.value)(b._listSpaced(p.primary(e)))),a.lumiElement(p.banner)(y._alignSelf(y.Stretch.value)(b._listSpaced(p.active(e)))),a.lumiElement(p.banner)(y._alignSelf(y.Stretch.value)(b._listSpaced(p.warning(e)))),a.lumiElement(p.banner)(y._alignSelf(y.Stretch.value)(b._listSpaced(p.error(e))))]},i=[l.button({accessibilityLabel:l.primary.accessibilityLabel,color:l.primary.color,onPress:l.primary.onPress,size:l.primary.size,style:l.primary.style,testId:l.primary.testId,title:"Try again",type:l.primary.type,buttonState:l.primary.buttonState}),l.button({accessibilityLabel:l.linkStyle.accessibilityLabel,color:l.linkStyle.color,onPress:l.linkStyle.onPress,size:l.linkStyle.size,style:l.linkStyle.style,testId:l.linkStyle.testId,title:"View error",type:l.linkStyle.type,buttonState:l.linkStyle.buttonState})],c.intercalate(c.foldableArray)(S.monoidJSX)(m.vspace(m.S8.value))([d.p_("Banners are small pieces of informative content that draw the attention of the user through color and that may require user action."),d.h2_("round (default), spaced list, non-dismissable"),u.example(S.fragment(r((function(e){return{css:e.css,className:e.className,component:e.component,dismissable:e.dismissable,content:s}})))),d.h2_("round (default), spaced list, dismissable"),u.example(S.fragment(r((function(e){return{css:e.css,className:e.className,component:e.component,dismissable:!0,content:o}})))),d.h2_("action banner, round (default), spaced list, non-dismissable"),u.example(S.fragment(r(p.actionBanner(i)((function(e){return{css:e.css,className:e.className,component:e.component,dismissable:e.dismissable,content:s}}))))),d.h2_("action banner, round (default), spaced list, dismissable"),u.example(S.fragment(r(p.actionBanner(i)((function(e){return{css:e.css,className:e.className,component:e.component,dismissable:!0,content:o}})))))]));e.exports={docs:k}},xEpL:function(e,n,t){"use strict";var s,o,r=t("5a5/"),i=t("bqfS"),c=t("g4Rk"),a=t("eAL6"),l=t("i/4q"),u=t("vKy7"),m=(s=i.styleModifier((function(e){return r.fold(r.foldableArray)(u.monoidStyle)([u.css()({backgroundColor:u.color(e.colors.black4),color:u.color(e.colors.black),padding:u.str("12px 16px")}),l.desktopQuery(u.css()({padding:u.str("12px 24px")}))])})),o=a._align(a.Center.value),function(e){return s(c._round(o(a._row(a.box(e)))))});e.exports={banner:m}}}]);