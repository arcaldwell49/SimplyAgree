
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"method1","title":"Method 1","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"method2","title":"Method 2","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"ciWidth","title":"Confidence level","type":"Number","min":0.5,"max":0.999,"default":0.95,"description":{"ui":"the confidence interval width.\n","R":"a number between .50 and .999 (default: .95), the width of confidence intervals\n"}},{"name":"agreeWidth","title":"Agreement level","type":"Number","min":0.5,"max":0.999,"default":0.95,"description":{"ui":"The agreement level\n","R":"a number between .50 and .999 (default: .95), the width of agreement limits\n"}},{"name":"testValue","title":"Test Value","type":"Number","default":2,"description":{"ui":"Value that would define adequate absolute agreement.","R":"a number specifying the limit of agreement"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Simple Test of Agreement in jamovi",
    jus: "3.0",
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
					label: "Method 1",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "method1",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Method 2",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "method2",
							maxItemCount: 1,
							isTarget: true
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
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "ciWidth",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "agreeWidth",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "testValue",
					format: FormatDef.number
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
