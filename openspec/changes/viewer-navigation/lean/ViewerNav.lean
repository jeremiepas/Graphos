/-
ViewerNav — formal validation of the viewer-navigation OpenSpec proposal.

Each module proves one requirement of specs/viewer-navigation/spec.md;
Scenarios.lean validates every spec scenario as an executable example.
-/
import ViewerNav.State
import ViewerNav.Codec
import ViewerNav.History
import ViewerNav.Breadcrumb
import ViewerNav.Keyboard
import ViewerNav.Path
import ViewerNav.Scenarios
