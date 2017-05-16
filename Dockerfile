FROM erlang:latest

LABEL description="Realtime bidding platform for the WWW"
LABEL version="0.1"
LABEL author="khanhhua"

WORKDIR /home/erbid/app-root

ADD https://s3.amazonaws.com/rebar3/rebar3 /home/erbid/rebar3
RUN chmod a+x /home/erbid/rebar3
ENV PATH=/home/erbid/rebar3/bin:$PATH

ADD . ./

RUN rm -rf _build/default && echo "Building with profile 'default'..." && rebar3 release

EXPOSE 8080
CMD ./_build/default/rel/erbid_alpha/bin/erbid_alpha foreground

