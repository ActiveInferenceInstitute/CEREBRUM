# Security Policy

## Supported Versions

Security fixes are applied to the current release of CEREBRUM. Older releases
are not maintained for security issues; users are encouraged to upgrade to the
latest version.

## Reporting a Vulnerability

Please **do not** open a public issue for security vulnerabilities.

To report a vulnerability privately, use GitHub's private vulnerability
reporting for this repository:

- https://github.com/ActiveInferenceInstitute/CEREBRUM/security/advisories/new

Reports should include:

- A description of the vulnerability and its impact
- Steps to reproduce (or a minimal proof of concept)
- Affected versions, if known

You can expect an acknowledgment within a few business days, and a follow-up
with the assessment and any remediation timeline.

## Scope

The CEREBRUM framework is research software. In particular, note that:

- API keys for LLM providers (e.g., `OPENROUTER_API_KEY`) are read from the
  environment and must never be committed to the repository.
- The repository is public; do not commit credentials, personal files, or
  internal tooling artifacts to it.
