const searchClient = algoliasearch("", "");

const search = instantsearch({
  indexName: "start-over",
  searchClient,
  searchFunction(helper) {
    // Ensure we only trigger a search when there's a query
    if (helper.state.query) {
      helper.search();
    }
  },
});

search.addWidgets([
  instantsearch.widgets.searchBox({
    container: "#searchbox",
  }),

  instantsearch.widgets.hits({
    container: "#hits",
    templates: {
      item: `
      <article>
        <a href="{{ permalink }}">
          <strong>
            {{#helpers.highlight}}
              { "attribute": "title", "highlightedTagName": "mark" }
            {{/helpers.highlight}}
          </strong>
        </a>
        {{#content}}
          <p>{{#helpers.snippet}}{ "attribute": "content", "highlightedTagName": "mark" }{{/helpers.snippet}}</p>
        {{/content}}
      </article>
    `,
    },
  }),
]);

search.start();
