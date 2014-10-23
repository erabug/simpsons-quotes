function randomSelection (object_length) {
    return Math.floor(Math.random() * object_length);
}

var seasons = {
    '1': [],
    '2': [],
    '3': [],
    '4': []
};

$(document).ready(function(){

    $.ajax({
        type: 'GET',
        url: 'quotes.json',
        dataType: 'json',
        success: function(data) {
            for (var i = 0; i < data.length; i++) {
                var str = data[i].season.split("");
                var season = str[str.length - 1];
                seasons[season].push(data[i]);
            }
        }
    });

    $('#getdata-button').on('click', function(){
        var quoteP = $('#quote');
        var detailsP = $('#details');

        var season = $('select[name="season-selection"]').val();
        if (season == 'all') {
            season = randomSelection(Object.keys(seasons).length) + 1;
        }
        var seasonQuotes = seasons[season];
        var entry = seasonQuotes[randomSelection(seasonQuotes.length)];

        quoteP.html("");
        detailsP.html("");

        if ($.isArray(entry.quote)) {
            // quoteP.html(entry.quote.join("</p><p>"));
            var lines = [];
            $.each(entry.quote, function(e, val) {
                var split = val.split(":");
                var speaker = '<div id="speaker">' + split[0] + ':</div>';
                var line = '<div id="line">' + split[1] + '</div>';
                lines.push('<div id="quote-container">' + speaker + line + '</div>');
            });
            // console.log(lines);
            $('#quote').html(lines);

        } else {
            quoteP.html(entry.quote);
            detailsP.html("-- " + entry.speaker);
        }
        $('#episode').html(entry.episode);
        $('#season').html(entry.season);
    });

});











