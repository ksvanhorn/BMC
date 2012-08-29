(use-package :compile)

(define-test compile-tests
  (assert-equal
"using System;
using Common;

namespace Foo
{
    [Serializable]
    public class Bar
    {
        <class body>
    }
}
"
    (ppstr (compile::write-csharp-class-skeleton
	     "Foo" "Bar" (lambda () (fmt "<class body>")))))
)
