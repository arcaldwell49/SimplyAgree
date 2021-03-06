
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"ui":"Data\n","R":"Data\n"}},{"name":"method1","title":"Method 1","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"rejectInf":false,"description":{"ui":"1st method of measurement\n","R":"Name of column containing 1st Vector of data\n"}},{"name":"method2","title":"Method 2","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"rejectInf":false,"description":{"ui":"2nd method of measurement\n","R":"Name of column containing Vector of data\n"}},{"name":"id","title":"Subject Identifier","type":"Variable","description":{"ui":"Column with subject identifier\n","R":"Name of column containing subject identifier\n"}},{"name":"ciWidth","title":"Confidence level (%)","type":"Number","min":50,"max":99.9,"default":95,"description":{"ui":"the confidence interval width.\n","R":"a number between 50 and 99.9 (default: 95), the width of confidence intervals\n"}},{"name":"agreeWidth","title":"Agreement level (%)","type":"Number","min":50,"max":99.9,"default":95,"description":{"ui":"The agreement level\n","R":"a number between 50 and 99.9 (default: 95), the width of agreement limits\n"}},{"name":"testValue","title":"Agreement bound (±)","type":"Number","default":2,"description":{"ui":"Value that would define adequate absolute agreement.","R":"a number specifying the limit of agreement"}},{"name":"CCC","title":"Concordance Correlation Coefficient (CCC)","type":"Bool","default":true,"description":{"R":"`TRUE` or `FALSE` (default), produce CCC table\n"}},{"name":"valEq","title":"Assume underlying value does not vary?","type":"Bool","default":false},{"name":"plotbland","title":"Bland-Altman plot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), for Bland-Altman plot\n"}},{"name":"plotcon","title":"Line-of-Identity plot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), for Line of identity plot\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Nested/Replicate Data Agreement Analysis",
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
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Subject Identifier",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "id",
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
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					cell: {"column":0,"row":0},
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "ciWidth",
							format: FormatDef.number
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "agreeWidth",
							format: FormatDef.number
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "testValue",
							format: FormatDef.number
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					cell: {"column":1,"row":0},
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "CCC"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "valEq"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "plotbland"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "plotcon"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
