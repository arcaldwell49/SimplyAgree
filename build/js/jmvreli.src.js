
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"vars","title":"Measurements","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"rejectInf":false,"description":{"ui":"Columns with the measured outcome.\n","R":"a list of the column names containing the measurements for reliability analysis.\n"}},{"name":"ciWidth","title":"Confidence Level","type":"Number","min":0.5,"max":0.999,"default":0.95,"description":{"ui":"the confidence interval width.\n","R":"a number between .50 and .999 (default: .95), the width of confidence intervals\n"}},{"name":"desc","title":"Variance Components","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide table of variance components\n"}},{"name":"plots","title":"Plot Data","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), plot data\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Reliability Analysis",
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
					label: "Measurements",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "vars",
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
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "desc"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "plots"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
