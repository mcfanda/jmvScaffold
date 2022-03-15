
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"dep","title":"Dependent Variable","type":"Variable","default":null,"suggested":["continuous","ordinal"],"permitted":["numeric"],"description":{"R":"a string naming the dependent variable from `data`, variable must be numeric\n"}},{"name":"somevars","title":"Some Variables","type":"Variables","default":null,"suggested":["continuous","nominal"],"permitted":["numeric"],"description":{"R":"a string naming the dependent variable from `data`, variable must be numeric\n"}},{"name":"correlations","title":"Correlations","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), shows correlation table\n"}},{"name":"regression","title":"Regression","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), shows correlation table\n"}},{"name":"focused","title":"Focal variable","type":"Terms","default":null},{"name":"test","title":"Test option","type":"Bool","default":false},{"name":"msg1","title":"Send a message at init","type":"Bool","default":false},{"name":"msg2","title":"Send a message at run","type":"Bool","default":false},{"name":"msg3","title":"Send a message at init and change it at run","type":"Bool","default":false},{"name":"residuals","title":"Residuals","type":"Output"}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	],

	update: require('./scaffold.events').update

    }).call(this);
}

view.layout = ui.extend({

    label: "jmvScaffold",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Dependent Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Other Variables",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "somevars",
							isTarget: true,
							events: [
								{ execute: require('./scaffold.events').onChange_vars }
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Focus on variables",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Supplier,
					typeName: 'Supplier',
					name: "focusedSupplier",
					label: "Variables",
					persistentItems: true,
					stretchFactor: 1,
					format: FormatDef.term,
					higherOrders: true,
					events: [
						{ onEvent: 'update', execute: require('./scaffold.events').onUpdate_focusedSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							transferAction: "interactions",
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "focused",
									valueFilter: "unique",
									isTarget: true,
									template:
									{
										type: DefaultControls.TermLabel,
										typeName: 'TermLabel'
									}									
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "correlations"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "regression"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "test"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "msg1"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "msg2"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "msg3"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.Output,
					typeName: 'Output',
					name: "residuals"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
