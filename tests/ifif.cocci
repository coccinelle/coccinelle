@@
@@

+#ifdef FOO1
+#ifdef FOO2
foo();
+#endif /* FOO1 */
+#endif /* FOO2 */
...
+#ifdef BAR1
+#ifdef BAR2
bar();
+#endif /* BAR1 */
+#endif /* BAR2 */
