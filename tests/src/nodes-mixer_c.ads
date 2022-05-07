with Testsuite; use Testsuite;
with Nodes.Gen_Mixer;

package Nodes.Mixer_C is new Nodes.Gen_Mixer (Cat_K     => Cat_C,
                                              Port_K    => Port_C,
                                              Node_Name => "mixer_c");
