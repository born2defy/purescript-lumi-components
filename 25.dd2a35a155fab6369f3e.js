(window.webpackJsonp=window.webpackJsonp||[]).push([[25],{Qqvi:function(e,t,a){"use strict";var s,l,i=a("H2/w"),r=a("U4xy"),n=a("aOgf"),c=a("6g2T"),u=a("5L+5"),m=a("0nsp"),o=a("TKTp"),d=a("8yhI"),g=a("qz/O"),f=a("pYS8"),p=a("PsKH"),h=a("pMgY"),b=a("7HZk"),y=a("sfHK"),x=a("h+YZ"),_=(s=[[h.body_("User"),f.row_([m.avatar({image:x.img()({src:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"}),size:p.Large.value,style:y.css({marginRight:"8px"})}),c.column_([h.body_("Flexo R."),h.text({children:[y.text("Lumi")],className:h.subtext.className,color:r.toNullable(new i.Just(n.colorNames.black1)),style:y.css({}),tag:h.subtext.tag,testId:h.subtext.testId})])])],[h.body_("ID"),o.link({className:o.defaults.className,href:"/",navigate:o.defaults.navigate,style:y.css({}),target:o.defaults.target,testId:o.defaults.testId,text:h.body_("1234")})],[h.body_("Created"),h.body_("2018-09-02")]],l=[{src:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg",name:"Flexo R.",companyName:"Lumi",createdDate:"2018-09-02",id:"123"},{src:"https://s3.amazonaws.com/lumi-flapjack-staging/avatars/vfhpnqairr/thumbnail_1517878176349.png",name:"Bob Sagat",companyName:"Full House",createdDate:"2018-09-02",id:"456"},{src:"https://s3.amazonaws.com/lumi-flapjack-staging/avatars/vfhpnqairr/thumbnail_1517878176349.png",name:"David Bowie",companyName:"Musician",createdDate:"2018-09-02",id:"789"}],c.column_([h.h2_("Basic List"),u.example(c.columnSelfStretch([d.list({size:d.defaultList.size,rightAligned:d.defaultList.rightAligned,rows:s})])),h.h2_("Compact Basic List"),u.example(c.columnSelfStretch([d.list({size:d.compactList.size,rightAligned:d.compactList.rightAligned,rows:s})])),h.h2_("Right-aligned (last column) Basic List"),u.example(c.columnSelfStretch([d.list({size:d.defaultList.size,rightAligned:!0,rows:s})])),h.h2_("Structured Column List"),u.example(c.columnSelfStretch([d.structuredColumnList({rightAligned:!1,rows:l,columns:[{renderCell:function(e){return g.lockup({title:y.text(e.name),subtitle:i.Just.create(y.text(e.companyName)),image:i.Just.create(m.avatar_({size:p.Large.value,image:x.img()({src:e.src})}))})}},{renderCell:function(e){return h.body_(e.createdDate)}},{renderCell:function(e){return h.body_(e.id)}}]})])),h.h2_("Right-aligned (last column) Structured Column List"),u.example(c.columnSelfStretch([d.structuredColumnList({rightAligned:!0,rows:l,columns:[{renderCell:function(e){return g.lockup({image:i.Just.create(m.avatar_({size:p.Large.value,image:x.img()({src:e.src})})),title:y.text(e.name),subtitle:i.Just.create(y.text(e.companyName))})}},{renderCell:function(e){return h.body_(e.createdDate)}},{renderCell:function(e){return b.fragment([o.link({className:o.defaults.className,href:"/",navigate:o.defaults.navigate,style:y.css({marginRight:"16px"}),target:o.defaults.target,testId:o.defaults.testId,text:h.body_("Edit")}),o.link({className:o.defaults.className,href:"/",navigate:o.defaults.navigate,style:o.defaults.style,target:o.defaults.target,testId:o.defaults.testId,text:h.body_("Deactivate")})])}}]})]))]));e.exports={docs:_}}}]);