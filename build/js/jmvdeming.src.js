
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"ui":"Data\n","R":"Data\n"}},{"name":"method1","title":"Method 1","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"rejectInf":false,"description":{"ui":"1st method of measurement\n","R":"Name of column containing 1st Vector of data\n"}},{"name":"method2","title":"Method 2","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"rejectInf":false,"description":{"ui":"2nd method of measurement\n","R":"Name of column containing Vector of data\n"}},{"name":"ciWidth","title":"Confidence level (%)","type":"Number","min":50,"max":99.9,"default":95,"description":{"ui":"the confidence interval width.\n","R":"a number between 50 and 99.9 (default: 95), the width of confidence intervals\n"}},{"name":"testValue","title":"Error Ratio between X & Y","type":"Number","default":1,"description":{"ui":"Ratio of the two error variances. Default is 1.","R":"Ratio of the two error variances. Default is 1."}},{"name":"plotcon","title":"Line-of-Identity plot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), for Bland-Altman plot\n"}},{"name":"plotcheck","title":"Check assumptions","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), assumptions plots\n"}},{"name":"weighted","title":"Weighted Deming regression","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE`\n"}},{"name":"xlabel","title":"Label for x-axis","type":"String","default":"Method: 1","description":{"ui":"The label for the x-axis\n","R":"The label for the x-axis\n"}},{"name":"ylabel","title":"Label for y-axis","type":"String","default":"Method: 2","description":{"ui":"The label for the y-axis\n","R":"The label for the y-axis\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Deming Regression",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			permitted: ["numeric"],
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
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Regression Analysis Settings",
			name: "loapanel",
			collapsed: false,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					style: "inline",
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
							name: "testValue",
							format: FormatDef.number
						}
					]
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "weighted"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Other Settings",
			name: "dispanel",
			collapsed: true,
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "plotcon"
				},
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "xlabel",
					width: "largest",
					format: FormatDef.string
				},
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "ylabel",
					width: "largest",
					format: FormatDef.string
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "plotcheck"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
