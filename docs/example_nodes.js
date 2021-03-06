function Node_CAT_A_print_a()
{
  that = this;
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.properties = {};
}
Node_CAT_A_print_a.title = "print_a";
LiteGraph.registerNodeType("CAT_A/print_a", Node_CAT_A_print_a);
function Node_CAT_B_print_b()
{
  that = this;
  this.addInput("In", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.properties = {};
}
Node_CAT_B_print_b.title = "print_b";
LiteGraph.registerNodeType("CAT_B/print_b", Node_CAT_B_print_b);
function Node_CAT_C_print_c()
{
  that = this;
  this.addInput("In", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.properties = {};
}
Node_CAT_C_print_c.title = "print_c";
LiteGraph.registerNodeType("CAT_C/print_c", Node_CAT_C_print_c);
function Node_CAT_A_input_a()
{
  that = this;
  this.addOutput("Out", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.properties = {};
}
Node_CAT_A_input_a.title = "input_a";
LiteGraph.registerNodeType("CAT_A/input_a", Node_CAT_A_input_a);
function Node_CAT_B_input_b()
{
  that = this;
  this.addOutput("Out", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.properties = {};
}
Node_CAT_B_input_b.title = "input_b";
LiteGraph.registerNodeType("CAT_B/input_b", Node_CAT_B_input_b);
function Node_CAT_C_input_c()
{
  that = this;
  this.addOutput("Out", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.properties = {};
}
Node_CAT_C_input_c.title = "input_c";
LiteGraph.registerNodeType("CAT_C/input_c", Node_CAT_C_input_c);
function Node_CAT_A_mixer_a()
{
  that = this;
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.addOutput("Out", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.properties = {};
}
Node_CAT_A_mixer_a.title = "mixer_a";
LiteGraph.registerNodeType("CAT_A/mixer_a", Node_CAT_A_mixer_a);
function Node_CAT_B_mixer_b()
{
  that = this;
  this.addInput("In", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.addInput("In", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.addInput("In", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.addInput("In", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.addOutput("Out", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.properties = {};
}
Node_CAT_B_mixer_b.title = "mixer_b";
LiteGraph.registerNodeType("CAT_B/mixer_b", Node_CAT_B_mixer_b);
function Node_CAT_C_mixer_c()
{
  that = this;
  this.addInput("In", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.addInput("In", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.addInput("In", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.addInput("In", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.addOutput("Out", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.properties = {};
}
Node_CAT_C_mixer_c.title = "mixer_c";
LiteGraph.registerNodeType("CAT_C/mixer_c", Node_CAT_C_mixer_c);
function Node_CAT_A_const_a()
{
  that = this;
  this.addOutput("Out", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.properties = {};
}
Node_CAT_A_const_a.title = "const_a";
LiteGraph.registerNodeType("CAT_A/const_a", Node_CAT_A_const_a);
function Node_CAT_B_const_b()
{
  that = this;
  this.addOutput("Out", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.properties = {};
}
Node_CAT_B_const_b.title = "const_b";
LiteGraph.registerNodeType("CAT_B/const_b", Node_CAT_B_const_b);
function Node_CAT_C_const_c()
{
  that = this;
  this.addOutput("Out", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.properties = {};
}
Node_CAT_C_const_c.title = "const_c";
LiteGraph.registerNodeType("CAT_C/const_c", Node_CAT_C_const_c);
function Node_CAT_A_bridge_a_b()
{
  that = this;
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.addOutput("Out", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.properties = {};
}
Node_CAT_A_bridge_a_b.title = "bridge_a_b";
LiteGraph.registerNodeType("CAT_A/bridge_a_b", Node_CAT_A_bridge_a_b);
function Node_CAT_B_bridge_b_c()
{
  that = this;
  this.addInput("In", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.addOutput("Out", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.properties = {};
}
Node_CAT_B_bridge_b_c.title = "bridge_b_c";
LiteGraph.registerNodeType("CAT_B/bridge_b_c", Node_CAT_B_bridge_b_c);
function Node_CAT_A_split_b_c()
{
  that = this;
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.addOutput("Out", "PORT_B", {shape: LiteGraph.BOX_SHAPE});
  this.addOutput("Out", "PORT_C", {shape: LiteGraph.ARROW_SHAPE});
  this.properties = {};
  this.properties["split_point"] =  0;
  this.addWidget("slider", "split_point",  0, null,
             { min: -100, max:  100, step: 1,
               property: "split_point"});
}
Node_CAT_A_split_b_c.title = "split_b_c";
LiteGraph.registerNodeType("CAT_A/split_b_c", Node_CAT_A_split_b_c);
function Node_CAT_A_widgets()
{
  that = this;
  this.addInput("In", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.addOutput("Out", "PORT_A", {shape: LiteGraph.ROUND_SHAPE});
  this.properties = {};
  this.properties["number_widget"] =  0;
  this.addWidget("number", "number_widget",  0, null,
             { min: -100, max:  100, step: 1,
               property: "number_widget"});
  this.properties["slider_widget"] =  0;
  this.addWidget("slider", "slider_widget",  0, null,
             { min: -100, max:  100, step: 1,
               property: "slider_widget"});
  this.properties["combo_widget"] =  0;
  this.addWidget("combo", "combo_widget",  0, null,
             { values:[
                   -5,
                   -4,
                   -3,
                   -2,
                   -1,
                    0,
                    1,
                    2,
                    3,
                    4,
                    5,
],
               property: "combo_widget"});
  this.properties["text_widget"] = "";
  this.addWidget("text", "text_widget", "", null,
               {property: "text_widget"});
  this.properties["toggle_widget"] = false;
  this.addWidget("toggle", "toggle_widget", false, null,
               {property: "toggle_widget"});
}
Node_CAT_A_widgets.title = "widgets";
LiteGraph.registerNodeType("CAT_A/widgets", Node_CAT_A_widgets);
function Node_CAT_A_print_prop()
{
  that = this;
  this.properties = {};
}
Node_CAT_A_print_prop.title = "print_prop";
LiteGraph.registerNodeType("CAT_A/print_prop", Node_CAT_A_print_prop);
