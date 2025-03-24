test_record = test.record
test_log = test.log

# http://practical-scheme.net/gauche/man/?l=en&p=test-record-file
test : lib/markdown/test.scm lib/markdown/inline_test.scm
	@rm -f $(test_record) $(test_log)
	$(foreach file,$^,GAUCHE_TEST_RECORD_FILE=$(test_record) gosh -I. $(file) >> $(test_log);)
	@cat $(test_record)

.PHONY : test
