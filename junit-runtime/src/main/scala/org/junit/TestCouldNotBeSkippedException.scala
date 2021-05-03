/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

class TestCouldNotBeSkippedException(cause: internal.AssumptionViolatedException)
    extends RuntimeException("Test could not be skipped due to other failures", cause)
