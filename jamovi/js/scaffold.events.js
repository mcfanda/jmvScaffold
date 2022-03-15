
const events = {
    update: function(ui) {
        console.log("update");
        updateFocusedSupplier(ui,this);

    },


    onChange_doNothing: function(ui) {
        console.log("do nothing");

    },
    
    onUpdate_focusedSupplier: function(ui) {
        console.log("do nothing");
    },

    onChange_vars: function(ui) {
        updateFocusedSupplier(ui,this);
    },
    onChange_focusedSupplier: function(ui) {
        let values = this.itemsToValues(ui.focusedSupplier.value());
        this.checkValue(ui.focused, false, values, FormatDef.variable);
    },


    
};

var updateFocusedSupplier=function(ui, context) { 
            let somevarsList = context.cloneArray(ui.somevars.value(), []);
            let depList = context.cloneArray(ui.dep.value(), []);
            var variablesList = depList.concat(somevarsList);
            ui.focusedSupplier.setValue(context.valuesToItems(variablesList, FormatDef.variable));
};

module.exports = events;
