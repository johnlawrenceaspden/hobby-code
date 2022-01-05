from django.shortcuts import render, get_object_or_404

# Create your views here.
from django.http import Http404
from django.http import HttpResponse

# from django.template import loader

from .models import Question


def index(request):
    latest_question_list = Question.objects.order_by("-pub_date")
    #   output = ", </br>".join([q.question_text for q in latest_question_list])
    #   template = loader.get_template("polls/index.html")
    context = {"latest_question_list": latest_question_list}
    return render(request, "polls/index.html", context)
    # return HttpResponse(output + template.render(context, request))


def detail(request, question_id):
    # try:
    #     question = Question.objects.get(pk=question_id)
    # except Question.DoesNotExist as e:
    #     raise Http404("Question does not exist") from e

    question = get_object_or_404(Question, pk=question_id)
    return render(request, "polls/detail.html", {"question": question})


def results(request, question_id):
    response = "You're looking at the results of question %s"
    return HttpResponse(response % question_id)


def vote(request, question_id):
    return HttpResponse("You're voting on question %s." % question_id)
