require 'rack'
module Effpee
  module HTTP
    module Simple
      def self.app
        Proc.new do |env|
          [ '200',
            {'Content-Type' => 'text/plain'},
            ['A barebones rack app.']
          ]
        end
      end
    end
  end
end


Rack::Handler::WEBrick.run Effpee::HTTP::Simple.app
