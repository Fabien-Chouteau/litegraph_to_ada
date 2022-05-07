with Testsuite; use Testsuite;
with Nodes.Gen_Mixer;

package Nodes.Mixer_A is new Nodes.Gen_Mixer (Cat_K     => Cat_A,
                                              Port_K    => Port_A,
                                              Node_Name => "mixer_a");
