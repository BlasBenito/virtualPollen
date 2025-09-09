## New version

This is a new version of the package (1.0.2) submitted as a response to an email by the CRAN Team:

------------------------------------------------------
Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_virtualPollen.html>.

Specifically, please see the NOTEs about Rd file(s) with Rd \link{}
targets missing package anchors in the "Rd cross-references" check.

CRAN is currently changing its package web pages to providing (static)
HTML refmans in addition to PDF refmans, which needs Rd cross-references
to Rd \link{} targets not in the package itself nor in the base packages
to use package anchors, i.e., use \link[PKG]{FOO} (see section
"Cross-references" in "Writing R Extensions"): otherwise these links
will not work.

Adding the missing package anchors should take very little time, so we
would really appreciate if you would do so in the next few weeks.

Please correct before 2025-09-01 to safely retain your package on CRAN.

Best wishes,
The CRAN Team
------------------------------------------------

I have fixed the conflicting links. All checks are clear now.


── R CMD check results ─────────── virtualPollen 1.0.2 ────
Duration: 1m 30s
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
