if (!dojo._hasResource["mydojo.TagCloud"]) { //_hasResource checks added by build. Do not use _hasResource directly in your code.
dojo._hasResource["mydojo.TagCloud"] = true;
dojo.provide("mydojo.TagCloud");

dojo.require("dijit._Templated");
dojo.require("dijit.layout.ContentPane");

dojo.declare("mydojo.TagCloud", [dijit.layout.ContentPane, dijit._Templated], {
	// pageSize: Integer
	//		Argument to data provider.
	//		Specifies number of search results per page (before hitting "next" button)
	pageSize: Infinity,
	// store: Object
	//		Reference to data provider object used by this ComboBox
	store: null,
	// query: Object
	//		A query that can be passed to 'store' to initially filter the items,
	//		before doing further filtering based on `searchAttr` and the key.
	//		Any reference to the `searchAttr` is ignored.
	query: {},
	// nameAttr: String
	//		The name of the tag attribute
	nameAttr: "name",
	// slugAttr: String
	//		The name of the slug attribute
	slugAttr: "slug",
	// countAttr: String
	//		The name of the number attribute
	countAttr: "count",
	// sizeDifference: Boolean
	//		If we should show larger font for more tags
	sizeDifference: true,
	// fontMaxSize: Integer
	//		The size of the largest tag in percent
	fontMaxSize: 200,
	// fontMinSize: Integer
	//		The size of the smallest tag in percent
	fontMinSize: 100,
	// weightDifference: Boolean
	//		If we should show bolder font for more tags
	weightDifference: false,
	// showTitle: Boolean
	//		If we should add a title attribute to the link of the tag
	showTitle: true,
	// showCount: Boolean
	//		If we should show the count next to the tag link. IE: tag (2)
	showCount: false,
	// clickFunction: String
	//		The name of the function we should call when a tag link is clicked
	clickFunction: "tagItemClicked",
	// baseClass: String
	//	The root className to use for this widget
	baseClass: "TagCloud",
	
	_tagMin: null,
	_tagMax: null,
	
	templateString:"<div class=\"${baseClass}\">\n</div>\n",
	
	postCreate: function(){
		var query = dojo.clone(this.query);
		var dataObject = this.store.fetch({
			queryOptions: {
				ignoreCase: true, 
				deep: true
			},
			query: query,
			onComplete: dojo.hitch(this, "_displayTags"), 
			onError: function(errText){
				console.error('mydojo.TagCloud: ' + errText);
			},
			start:0,
			count:this.pageSize
		});
	},
	
	_displayTags: function(/*Object*/ results, /*Object*/ dataObject){
		if(!this._tagMin){
			this._getMaxMin(results);
		}
		var oUL = dojo.doc.createElement("ul");
		dojo.place(oUL, this.domNode, "last");
		dojo.forEach(results,function(item){ 
			var count = parseInt(this.store.getValue(item, this.countAttr));
			var name = this.store.getValue(item, this.nameAttr);
			var slug = this.store.getValue(item, this.slugAttr);
			if(!slug || slug=="" || slug=="undefined"){
				slug = name;
			}
			var oLI = dojo.doc.createElement("li");
			dojo.place(oLI, oUL, "last");
			var oLink = dojo.doc.createElement("a");
			if(this.sizeDifference){
				oLink.style.fontSize = this._getFontSize(count) + '%';
			}
			if(this.weightDifference){
				oLink.style.fontWeight = this._getFontWeight(count);
			}
			if(this.showTitle){
				oLink.setAttribute("title", name + ' (' + count + ')');
			}
			oLink.setAttribute("href", 'javascript:' + this.clickFunction + '(\'' + slug + '\')');
			oLink.innerHTML = name;
			dojo.place(oLink, oLI, "last");
			if(this.showCount){
				dojo.place(dojo.doc.createTextNode(' (' + count + ')'), oLI, "last");
			}
			dojo.place(dojo.doc.createTextNode(' '), oUL, "last");
		},this);
	},
	
	_getMaxMin: function(/*Object*/ results){
		dojo.forEach(results,function(item){ 
			var count = parseInt(this.store.getValue(item, this.countAttr));
			if ((!this._tagMin) || (count < this._tagMin)) {
	  		this._tagMin = count;
	  	}
	  	if ((!this._tagMax) || (count > this._tagMax)) {
	  		this._tagMax = count;
	  	}
		},this); 
		this._slope = (this.fontMaxSize-this.fontMinSize)/(this._tagMax-this._tagMin);
		this._yintercept = (this.fontMinSize-((this._slope)*this._tagMin));
		this._weightSlope = (900-100)/(this._tagMax-this._tagMin);
		this._weightYIntercept = (100-((this._weightSlope)*this._tagMin));
	},
	
	_getFontWeight: function(count){
		return 100*(Math.round(((this._weightSlope*count)+this._weightYIntercept)/100));
	},
	
	_getFontSize: function(count){
		return (this._slope*count)+this._yintercept;
	}
});
	
}