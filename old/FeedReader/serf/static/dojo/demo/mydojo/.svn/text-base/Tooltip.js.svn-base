if (!dojo._hasResource["mydojo.Tooltip"]) { //_hasResource checks added by build. Do not use _hasResource directly in your code.
dojo._hasResource["mydojo.Tooltip"] = true;
dojo.provide("mydojo.Tooltip");
		dojo.require("dijit.form.Button");
	
		dojo.require("dojox.gfx");
		dojo.require("dojox.gfx.move");
		dojo.require("dijit._Widget"); dojo.require("dijit._Templated");
		
		dojo.declare("mydojo.Tooltip",[dijit._Widget,dijit._Templated],{
			
			// attachId: String|DomNode?
			// 		the Id or domNode to attach this tooltip to
			attachId:"",

			// attachHover: Boolean
			// 		disable hover behavior for the target
			attachHover:true,

			// attachParent: Boolean
			//		automatically attach to our parentnode rather than byId or query
			attachParent:false,

			// attachQuery: String?
			//		an optional selector query to attach this tooltip to
			attachQuery:"",

			// attachScope: String|DomNode?
			//		and optional scope to run the query against, passed as the
			//		second arg to dojo.query()
			queryScope:"",

			// hideDelay: Int
			// 		time in my to delay automatically closing the node
			hideDelay: 123, // ms

			// persists: Boolean
			//		if true, the node will stay visible until explicitly closed
			//		via _hide() or click on closeIcon
			persists:false,
			
			templateString:
				'<div class="foo">'
					+'<div style="position:relative;">'
						+'<div dojoAttachPoint="surfaceNode"></div>'
						+'<div class="tooltipBody" dojoAttachPoint="containerNode"></div>'
					+'</div>'
				+'</div>',
			
			postCreate:function(){
				// call _Widget postCreate first
				this.inherited(arguments);
				// gfx version of "_Templated" idea:
				this._initSurface();
				
				if(this.attachParent){
					// over-ride and reuse attachId as domNode from now on
					this.attachId = this.domNode.parentNode;
				}
				if(this.attachId){
					// domNode again. setup connections
					this.attachId = dojo.byId(this.attachId);
					if(this.attachHover){
						this.connect(this.attachId,"onmouseenter","_show");
					}
					if(!this.persists){
						this.connect(this.attachId,"onmouseleave","_initHide");
					}
				}else if(this.attachQuery){
					// setup connections via dojo.query for multi-tooltips
					var nl = dojo.query(this.attachQuery,this.queryScope);
					if(this.attachHover){ nl.connect("onmouseenter",this,"_show") }
					if(!this.persists){ nl.connect("onmouseleave",this,"_initHide") }
				}
				// place the tooltip			
				dojo.body().appendChild(this.domNode);
				dojo.style(this.domNode,{
					position:"absolute"
				});
				// could do this in css:
				dojo.style(this.containerNode,{
					position:"absolute",
					top:"15px",
					left:"12px",
					height:"83px",
					width:"190px"
				});
				// setup our animations
				this._hideAnim = dojo.fadeOut({ node:this.domNode, duration:150 });
				this._showAnim = dojo.fadeIn({ node:this.domNode, duration:75 });
				this.connect(this._hideAnim,"onEnd","_postHide");
				if(!this.persists){
					this.connect(this.domNode,"onmouseleave","_initHide");
				}
				// hide quickly
				this._postHide();
			},
			
			_initHide: function(e){
				// summary: start the timer for the hideDelay
				if(!this.persists && this.hideDelay){
					this._delay = setTimeout(dojo.hitch(this,"_hide",e||null),this.hideDelay);
				}
			},
			
			_clearDelay: function(){
				// summary: clear our hide delay timeout
				if(this._delay){ clearTimeout(this._delay); }
			},
			
			_show: function(e){
				dojo.style(this.domNode,"zIndex","888");
				// summary: show the widget
				this._clearDelay();
				var pos = dojo.coords(e.target || this.attachId,true)
				// we need to more accurately position the domNode:
				dojo.style(this.domNode,{
					top: pos.y + 50,
					left: pos.x + 50,
					display:"block"
				});
				dojo.fadeIn({ node: this.domNode, duration:75 }).play(true);
			},
			
			_hide: function(e){
				// summary: hide the tooltip
				this._hideAnim.play();
			},
			
			_postHide: function(){
				// summary: after hide animation cleanup
				dojo.style(this.domNode,"display","none");
			},
			
			_initSurface:function(){
				// made generally from an SVG file:
				this.surface = dojox.gfx.createSurface(this.surfaceNode,220,120);
				this.tooltip = this.surface.createGroup();
				this.tooltip.createPath("M213,101.072c0,6.675-5.411,12.086-12.086,12.086H13.586 c-6.675,0-12.086-5.411-12.086-12.086V21.004c0-6.675,5.411-12.086,12.086-12.086h187.328c6.675,0,12.086,5.411,12.086,12.086 V101.072z")
					.setFill("rgba(0,0,0,0.25)");
				
				this.tooltip.createPath("M211.5,97.418c0,6.627-5.373,12-12,12 h-186c-6.627,0-12-5.373-12-12v-79.5c0-6.627,5.373-12,12-12h186c6.627,0,12,5.373,12,12V97.418z")
					.setStroke({ width:2, color:"#FFF" })
					.setFill("rgba(102,102,153,1)")
					.connect("onmouseover",dojo.hitch(this,"_clearDelay"));
				
				if(this.persists){
					// make the close icon
					this._toolButton = this.surface.createGroup();
					this._toolButton.createEllipse({ cx:207.25, cy:12.32, rx: 7.866, ry: 7.099 })
						.setFill("#ededed");
					this._toolButton.createCircle({ cx:207.25, cy: 9.25, r:8.25 })
						.setStroke({ width:2, color:"#FFF" })
						.setFill("#000")
					;
					this._toolButton.connect("onclick",dojo.hitch(this,"_hide"));	
					// the X	
					this._toolButton.createLine({ x1:203.618, y1:5.04, x2: 210.89, y2:12.979 })
						.setStroke({ width:2, color:"#d6d6d6" });
					this._toolButton.createLine({ x1:203.539, y1:12.979, x2: 210.89, y2:5.04 })
						.setStroke({ width:2, color:"#d6d6d6" });
				}
			}	
		});
}