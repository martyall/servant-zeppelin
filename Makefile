.PHONY: hlint stylish

hlint:
	hlint servant-zeppelin "--ignore=Parse error" -XTypeApplications; \
		hlint servant-zeppelin-server "--ignore=Parse error" -XTypeApplications
		hlint servant-zeppelin-swagger "--ignore=Parse error" -XTypeApplications

stylish:
	find ./servant-zeppelin -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
		find ./servant-zeppelin-server -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
		find ./servant-zeppelin-swagger -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
